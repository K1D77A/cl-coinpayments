(in-package :cl-coinpayments)

;;;;this file contains some helpers for working with the coinpayments.net api.

(defparameter *sym->string* (make-hash-table :test #'equal)
  "A list of symbols and their string counterparts")

(defun new-string (symbol)
  (setf (gethash (symbol-name symbol) *sym->string*)
        (str:snake-case (symbol-name symbol))))

(defun symbol->string (symbol)
  (let ((sym (gethash (symbol-name symbol) *sym->string*)))
    (if sym
        sym
        (new-string symbol))))

(defclass request ()
  ((version
    :accessor version
    :initarg :version
    :initform "1"
    :type string
    :documentation "The API version.")
   (dex-alist
    :accessor dex-alist
    :type list
    :documentation "A computed a list of all post vars for sending as form-urlencoded.")
   (post-string
    :accessor post-string
    :type string
    :documentation "The computed post string.")
   (api-secret-key
    :accessor api-secret-key
    :initarg :api-secret-key
    :type string
    :documentation "The private key used to sign the requests.")
   (key
    :accessor key
    :initarg :key
    :type string
    :documentation "The users public key")
   (cmd
    :accessor cmd
    :initarg :cmd
    :type string
    :documentation "The API being called.")
   (nonce
    :accessor nonce
    :initarg :nonce
    :type string
    :documentation "An optional nonce that should be one higher than the previous")
   (format
    :accessor response-format
    :initarg :format
    :initform "json"
    :type string
    :documentation "The response format. Default JSON.")
   (hmac
    :accessor hmac
    :initarg :hmac
    :type string
    :documentation "The computed hmac of the request.")
   (required
    :accessor required
    :initarg :required
    :initform nil
    :type list
    :documentation "A list of required slots."))
  (:documentation "The base class for all API requests."))

(defmacro new-request (name cmd required &rest params)
  `(defclass ,name (request)
     ((cmd :initform ,cmd)
      (required :initform ',(append required '(api-secret-key key)))
      ,@(loop :for param :in params
              :do (print param)
              :collect `(,param
                         :accessor ,param
                         :initarg ,(intern (string-upcase param) :keyword)
                         :type string)))
     ))

(new-request currency-prices "rates" () short)

(defmethod initialize-instance :after ((request request)
                                       &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (validate-slots request)
  (convert-api-secret-key request)
  (compute-dex-alist request)
  (compute-post-params request))

(defun convert-api-secret-key (request)
  (with-accessors ((key api-secret-key))
      request
    (when (stringp key)
      (make-array (length key) :element-type '(unsigned-byte 8)
                               :initial-contents (babel:string-to-octets key)))))

(defun validate-slots (request)
  (with-slots (required)
      request
    (when required 
      (unless (every (lambda (slot)
                       (print slot)
                       (slot-boundp request slot))
                     required)
        (let ((unbound
                (loop :for slot :in required
                      :unless (slot-boundp request slot)
                        :collect slot)))
          (error 'required-slots-not-bound :not-set unbound
                                           :required required))))))


(defmethod compute-hmac ((api-secret-key array) (string string))
  (ironclad:byte-array-to-hex-string
   (ironclad::hkdf-extract 'ironclad:sha512 api-secret-key string)))

(defmethod compute-hmac ((api-secret-key string) (string string))
  (let ((pk (babel:string-to-octets api-secret-key))
        (str (babel:string-to-octets string)))
    (compute-hmac
     (make-array (length pk) :element-type '(unsigned-byte 8)
                             :initial-contents pk)
     (make-array (length str) :element-type '(unsigned-byte 8)
                              :initial-contents str))))

(defun compute-dex-alist (request)
  (let* ((slots (c2mop:class-slots (class-of request))))
    (setf (dex-alist request)
          (loop :for slot :in slots
                :for name := (c2mop:slot-definition-name slot)
                  :then (c2mop:slot-definition-name slot)
                :unless (or (find name '(dex-alist required api-secret-key))
                            (not  (slot-boundp request name)))
                  :collect (cons (symbol->string name)
                                 (slot-value request name))))
    request))

(defun compute-post-params (request)
  (let* ((stream (make-string-output-stream)))
    (loop :for (key . val) :in (dex-alist request)
          :do (format stream "~A=~A&" key val))
    (let ((post (get-output-stream-string stream)))
      (setf (post-string request) (subseq  post 0 (1- (length post))))))
  request)

(defun compuete-final-hmac (request)
  (setf (hmac request)
        (compute-hmac (api-secret-key request) (post-string request)))
  request)
