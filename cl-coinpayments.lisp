;;;; cl-coinpayments.lisp

(in-package #:cl-coinpayments)


(defgeneric parse-data (data)
  (:documentation "Attempts to parse 'data' into a plist with keys that match the 
IPN class definitions in classes.lisp."))

(defmethod parse-data ((data list))
  (loop :for list :in data
        :appending (destructuring-bind (key . val)
                       list
                     (list (intern (string-upcase (str:param-case key)) :keyword)
                           val))))

(defmethod parse-data ((data string))
  "Parse a www-url-form-encoded string into something that can be used to 
   make a coinpayment ipn object."
  (let ((data (str:split #\& data)))
    (loop :for key-val :in data
          :appending (destructuring-bind (key val)
                         (str:split #\= key-val)
                       (list (intern (string-upcase (str:param-case key)) :keyword) val)))))

(defgeneric verify-data (hmac private-key raw-post)
  (:documentation "A generic function to verify the integrity of a request while
PRIVATE-KEY and RAW-POST are in different formats. Depending on the request, 
I think PRIVATE-KEY is either meant to be your private-api key, or your private IPN key.
IPN key for validating IPN's and private api key for sending requests to the API."))

(defmethod verify-data (hmac private-key raw-post)
  (string= hmac 
           (ironclad:byte-array-to-hex-string
            (ironclad::hkdf-extract 'ironclad:sha512 private-key raw-post))))

(defmethod verify-data (hmac (private-key string) (raw-post array))
  (let ((pk-b (babel:string-to-octets private-key)))
    (verify-data hmac pk-b raw-post)))

(defmethod verify-data (hmac private-key (raw-post string))
  (let ((rp-b (babel:string-to-octets raw-post)))
    (verify-data hmac private-key rp-b)))

(defun determine-unknown-status (n)
  "After calling CONSTRUCT-STATUS this is called in an attempt to determine whether the 
status is part of the API not implemented yet."
  (cond ((< n 0)
         (make-instance 'ipn-failure))
        ((<= 0 n 99)
         (make-instance 'ipn-payment-pending))
        ((>= n 100)
         (make-instance 'ipn-payment-sucess))
        (t (error 'unknown-status :status n))))

(defun construct-status (n)
  "Given a number N attempts to determine which IPN-STATUS object to create,
you can see all of the available status classes within classes.lisp. If it cannot match
the numbers specifically then more general classes are used based on the coinpayments
'loose' recommendations for future proofing, this is done with DETERMINE-UNKNOWN-STATUS."
  (case n
    (-2 (make-instance 'negative-2))
    (-1 (make-instance 'negative-1))
    (0 (make-instance 'zero))
    (1 (make-instance 'one))
    (2 (make-instance 'two))
    (3 (make-instance 'three))
    (5 (make-instance 'five))
    (100 (make-instance 'one-hundred))
    (otherwise (determine-unknown-status n))))

(defun construct-ipn (ipn-as-key plist)
  "Given a plist of post a parameters that should have been parsed by parse-data first,
attempts to construct an IPN object. If it cannot then signals 'unsupported-ipn."
  (apply #'make-instance 
         (case ipn-as-key
           (:SIMPLE 'ipn-simple-button)
           (:DEPOSIT 'ipn-deposit)
           (:WITHDRAWAL 'ipn-withdrawal)
           (:BUTTON 'ipn-advanced-button)
           (:DONATION 'ipn-donation) 
           (otherwise (error 'unsupported-ipn :ipn ipn-as-key :plist plist)))
         plist))

(defun construct-ipn-and-status (plist)
  "Given a plist of post parameters that have been parsed by parse-data beforehand,
attempts to first construct and IPN object, in the event this fails it will signal 
'unsupported-ipn, then attempts to construct an IPN-STATUS object using CONSTRUCT-STATUS.
These are then returned as multiple values ipn,status."
  (let ((ipn (getf plist :ipn-type))
        (status (parse-integer (getf plist :status) :junk-allowed t)))
    (when (and ipn status)
      (let ((ipn-key (intern (string-upcase ipn) :keyword)))
        (values (construct-ipn ipn-key plist)
                (construct-status status))))))








;;;these funs were just used to parse the ipn description html.
(defun field->slot (field)
  (destructuring-bind (&key field desc &allow-other-keys)
      field
    (format t "(~A~%:documentation \"~A\"~%:initarg ~S)~%"
            field desc (intern (symbol-name field) :keyword))))

(defun fields-to-slots (string)
  (let ((split-newline (str:split #\Newline string :omit-nulls t)))
    (let ((slots
            (loop :for list :in 
                            (mapcar (lambda (string)
                                      (str:split #\Tab string))
                                    split-newline)
                  :collect (destructuring-bind (field description required)
                               list
                             (list :field (intern (string-upcase (str:param-case field)))
                                   :desc description
                                   :required (if (string= required "Yes")
                                                 t
                                                 nil))))))
      (mapc #'field->slot slots))))

