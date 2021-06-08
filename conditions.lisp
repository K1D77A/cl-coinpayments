(in-package #:cl-coinpayments)

(define-condition coinpayment-condition ()
  ())

(define-condition unknown-status (coinpayment-condition)
  ((status
    :accessor status
    :initarg :status
    :documentation "The unknown status code."))
  (:documentation "signalled when an unknown status code is received.")
  (:report
   (lambda (obj stream)
     (format stream "Unknown status received from IPN. Status: ~A."
             (status obj)))))

(define-condition unsupported-ipn (coinpayment-condition)
  ((ipn
    :accessor ipn
    :initarg :ipn
    :documentation "The IPN that was received but isn't supported.")
   (plist
    :accessor plist
    :initarg :plist
    :documentation "The plist used to try and construct an IPN."))
  (:documentation "Signalled when an attempt was made to construct an unsupported IPN.")
  (:report
   (lambda (obj stream)
     (format stream "An IPN was received that is unknown. IPN: ~A. See (plist <condition>) ~
                    for the original post data."
             (ipn obj)))))
