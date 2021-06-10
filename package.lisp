;;;; package.lisp

(defpackage #:cl-coinpayments
  (:use #:cl)
  (:export #:ipn-status
           #:ipn-failure
           #:ipn-payment-pending
           #:ipn-payment-success
           #:negative-2
           #:negative-1
           #:zero
           #:one
           #:two
           #:three
           #:five
           #:one-hundred
           ;;classes ^
           ;;conditions
           #:coinpayment-condition
           #:unknown-status
           #:unsupported-ipn
           #:no-dispatcher-found
           ;;condition accessors
           #:ipn
           #:plist
           #:name
           #:status
           #:arg-count
           #:ipn-type
           #:status-type
           ;;main variables
           #:*ipn-dispatchers*
           ;;funs and macros
           #:new-ipn-dispatcher
           #:def-ipn-dispatcher
           #:find-dispatcher
           #:ipn-dispatch
           #:dispatch-ipn-by-name
           #:parse-data
           #:verify-data
           #:construct-status
           
           ))
