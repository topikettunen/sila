(defpackage #:sila/conditions
  (:use #:cl)
  (:export #:lexer-error))
(in-package #:sila/conditions)

(defun format-lexer-error (stream pos input msg)
  (format stream "Lexer error:~%~%~a~%~{a~}^ ~a"
          input (make-list pos :initial-element #\Space) msg))

(define-condition lexer-error (error)
  ((token :initarg :token
          :initform nil
          :reader token)
   (token-position :initarg :token-position
                   :initform nil
                   :reader token-position)
   (error-msg :initarg :error-msg
              :initform nil
              :reader error-msg)
   (lexer-input :initarg :lexer-input
                :initform nil
                :reader lexer-input))
  (:report (lambda (condition stream)
             (format-lexer-error stream
                                 (token-position condition)
                                 (lexer-input condition)
                                 (error-msg condition))))
  (:documentation "Condition for when we encounter invalid token."))
