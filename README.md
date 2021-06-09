# cl-coinpayments
This is simply a helper library for using working with the original version of the 
coinpayment.net API, there is a new version of the API in the works.

Currently no API calls from server->API are implemented, only API->server, the IPN system.

## Intro
The coinpayment IPN (Instant Payment Notification) system sends messages to a listening 
server to inform the server of activity within their coinpayment account, whether this is 
to inform the user of a processed payment, whether a payment has failed etc. 
You can see the current docs on IPN's here: https://www.coinpayments.net/merchant-tools-ipn
this library just helps with managing the IPN's you receive.

## Quick notes

The post data sent from the coinpayment server is formatted in snake case ie 
`i_am_a_variable` but when you convert the data into a plist representing an IPN, 
all the snake_case keys are converted to kebab case ie i-am-a-variable, so 
`ipn_type` would be the keyword `:IPN-TYPE`. Parsing is memoized so it should be
pretty fast.

## Parsing the IPN's

There is a generic function called `(parse-data data)` which accepts a list, a string or an 
array, with this it attempts to convert it to a PLIST.

```lisp
#(97 109 111 117 110 116 61 49 48 46 48 48 48 48 48 48 48 48 38 97 109 111 117
  110 116 105 61 49 48 48 48 48 48 48 48 48 48 38 99 117 114 114 101 110 99 121
  61 76 84 67 84 38 102 101 101 61 48 46 48 48 48 48 48 48 48 48 38 102 101 101
  105 61 48 38 102 105 97 116 95 97 109 111 117 110 116 61 49 49 48 56 46 48 51
  48 50 57 49 53 48 38 102 105 97 116 95 97 109 111 117 110 116 105 61 49 49 48
  56 48 51 48 50 57 49 53 48 38 102 105 97 116 95 99 111 105 110 61 71 66 80 38
  102 114 111 109 61 56 97 99 57 53 50 56 100 49 57 49 102 56 102 98 51 102 53
  98 57 100 97 97 50 53 53 51 56 99 52 97 101 38 105 112 110 95 105 100 61 102
  54 48 102 55 51 57 49 101 99 54 48 54 100 50 54 51 49 101 54 100 53 51 49 53
  55 57 48 54 100 98 102 38 105 112 110 95 109 111 100 101 61 104 109 97 99 38
  105 112 110 95 116 121 112 101 61 116 114 97 110 115 102 101 114 38 105 112
  110 95 118 101 114 115 105 111 110 61 49 46 48 38 109 101 114 99 104 97 110
  116 61 54 57 56 57 48 54 51 98 99 53 101 48 102 52 51 99 49 51 57 102 56 100
  100 102 101 48 101 55 49 100 98 57 38 115 116 97 116 117 115 61 50 38 115 116
  97 116 117 115 95 116 101 120 116 61 67 111 109 112 108 101 116 101 38 116
  120 110 95 105 100 61 67 84 70 70 51 84 77 74 73 57 79 74 65 73 74 56 83 83
  77 74 86 75 69 66 72 74)
CL-COINPAYMENTS> (parse-data *)
(:AMOUNT "10.00000000" :AMOUNTI "1000000000" :CURRENCY "LTCT" :FEE "0.00000000"
 :FEEI "0" :FIAT-AMOUNT "1108.03029150" :FIAT-AMOUNTI "110803029150" :FIAT-COIN
 "GBP" :FROM "8ac9528d191f8fb3f5b9daa25538c4ae" :IPN-ID
 "f60f7391ec606d2631e6d53157906dbf" :IPN-MODE "hmac" :IPN-TYPE "transfer" ... )
 ```

## Verifying the source

coinpayment signs all its messages from the API with a HMAC header, with Hunchentoot you 
can extract that header like so: 

```lisp
 (let* ((headers (tbnl:headers-in*))
        (hmac (cdr (assoc :hmac headers))))
     hmac)
 ```
 
Now using the method `(verify-data hmac private-key raw-post)`
you can parse this header your IPN Secret, this is 
not your API secret key but the key *you* provided as a 'secret' 
"Your IPN Secret is a string of your choosing that is used to verify that an IPN was really sent from our servers "

and either the parsed plist, the raw-data or a string consisting of post parameters like so:

```lisp
  (let* ((headers (tbnl:headers-in*))
         (hmac (cdr (assoc :hmac headers)))
         (raw-data (tbnl:raw-post-data))
         (plist (cl-coinpayments::parse-data (tbnl:post-parameters*))))
    (when (and (string= (getf plist :merchant) *coinpayment-merchant-id*)
               (cl-coinpayments:verify-data hmac *coinpayment-ipn-secret*
                                             raw-data))
        <do something>
        ))
```

In the above example I also confirm that my merchants ID and the merchants ID sent are
the same.

## Working with the IPN

Now you have verified the legitimacy of your IPN you can construct a status object.

```lisp
CL-COINPAYMENTS> (construct-status *)
#<TWO {10196A08D3}>
(:AMOUNT "10.00000000" :AMOUNTI "1000000000" :CURRENCY "LTCT" :FEE "0.00000000"
 :FEEI "0" :FIAT-AMOUNT "1108.03029150" :FIAT-AMOUNTI "110803029150" :FIAT-COIN
 "GBP" :FROM "8ac9528d191f8fb3f5b9daa25538c4ae" :IPN-ID
 "f60f7391ec606d2631e6d53157906dbf" :IPN-MODE "hmac" :IPN-TYPE "transfer"
 :IPN-VERSION "1.0" :MERCHANT "oof" :STATUS "2" :STATUS-TEXT "Complete" :TXN-ID
 "CTFF3TMJI9OJAIJ8SSMJVKEBHJ")
 ```
 
This returns two values, with the most easily referenced being the new status object.
The status objects rules are described here: https://www.coinpayments.net/merchant-tools-ipn 
If the status cannot be determined (which probably means the request is bogus) 
the condition 'unknown-status is signalled.

Under the heading 'Payment Statuses'. If you look in classes.lisp you can see the 
relationship between the status objects, CLOS is used to represent the relationship 
described in those docs, so the class one-hundred is a subclass of ipn-payment-success
and ipn-payment-success is a subclass of ipn-status etc, this is important for the next
part.
 
## Dispatching on statuses

This library has a means of creating functions that are executed based on the 
name of the dispatcher, the type of IPN sent from the server and the two main args the
class of the STATUS object and the number of arguments.
This is best demonstrated with an example:
```lisp

(def-ipn-dispatcher print-info ((foo :transfer) (ipn ipn-status) arg1)
  (print foo)
  (print ipn)
  (print "less specific")
  (print arg1))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn ipn-status) arg1 arg2)
  (print foo)
  (print ipn)
  (print "less specific")
  (print arg1)
  (print arg2))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn two) arg1)
  (print foo)
  (print ipn)
  (print "more specific"))

(def-ipn-dispatcher print-info ((foo :transfer) (ipn two) arg1 arg2)
  (print arg1)
  (print arg2))

  ```
Some example inputs using the function `(ipn-dispatch name ipn status args)`
```
CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'zero) "abc")

(:IPN-TYPE "transfer") 
#<ZERO {1025E64EB3}> 
"less specific" 
"abc" 
"abc"


CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'zero) "abc" "deeef")

(:IPN-TYPE "transfer") 
#<ZERO {1025E852C3}> 
"less specific" 
"abc" 
"deeef" 
"deeef"


CL-COINPAYMENTS> (ipn-dispatch 'print-info '(:IPN-TYPE "transfer") (make-instance 'two) "abc" "deeef")

"abc" 
"deeef" 
"deeef"

```
I assume you get the jist. Anyway you can use this to perform actions based the same types 
of IPN's but when they are in varying states.
There is the macro `(dispatch-ipn-by-name name ipn status args)` which does the same 
```lisp
CL-COINPAYMENTS> (dispatch-ipn-by-name print-info '(:IPN-TYPE "transfer") (make-instance 'ipn-status) "abc" "def")

(:IPN-TYPE "transfer") 
#<IPN-STATUS {1025E8F173}> 
"less specific" 
"abc" 
"def" 
"def"
```

If a dispatcher cannot be found for the args provided then the condition 
'no-dispatcher-found is signalled, you should wrap all calls to dispatch-ipn and 
dispatch-ipn-by-name within a handler-case to make sure you have some fallback 
functionality in the case the server sends something unexpected, which it has done for me...

Just a note the list '(:IPN-TYPE "transfer") is the most basic of IPN's represented as a
plist that is required for this to function, this is why it is used in the examples, in 
a real world example the IPN would be what has been received and parsed by (parse-data ..)



## License

MIT

