;;;; cl-coinpayments.asd

(asdf:defsystem #:cl-coinpayments
  :description "Helpers for working with the coinpayments.net api."
  :author "K1D77A"
  :license  "MIT"
  :version "1.0.1"
  :depends-on (#:str
               #:ironclad
               #:babel
               #:dexador
               #:jonathan
               #:closer-mop)
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "classes")
               (:file "cl-coinpayments")
               (:file "api-helpers")
               (:file "api-forms")))
