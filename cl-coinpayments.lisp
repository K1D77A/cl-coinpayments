;;;; cl-coinpayments.lisp

(in-package #:cl-coinpayments)




(defun parse-url-encoded (string)
  "Parse a www-url-form-encoded string into something that can be used to 
make a coinpayment ipn object."
  (let ((data (str:split #\& string)))
    (loop :for key-val :in data
          :appending (destructuring-bind (key val)
                         (str:split #\= key-val)
                       (list (intern (string-upcase key) :keyword) val)))))

;;;these funs were just used to parse the ipn description html.
(defun field->slot (field)
  (destructuring-bind (&key field desc &allow-other-keys)
      field
    (format t "(~A~%:documentation \"~A\"~%:initarg ~S)~%" field desc (intern (symbol-name field) :keyword))))

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

