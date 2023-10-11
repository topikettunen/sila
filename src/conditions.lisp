(defpackage #:sila/conditions
  (:use #:cl))
(in-package #:sila/conditions)

(defun format-lexer-error (stream pos input msg)
  "Print lexer error in format like:

Lexer error:

1+a1
  ^ msg
"
  (format stream "Lexer error:~%~%~a~%~{~a~}^ ~a"
          input (make-list pos :initial-element #\Space) msg))

(define-condition lexer-error (error)
  ((token-position :initarg :token-position
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

(define-condition parser-error (error)
  ((error-msg :initarg :error-msg
              :initform nil
              :reader error-msg))
  (:report (lambda (condition stream)
             (if (error-msg condition)
                 (format stream "~a~%" (error-msg condition))
                 (format stream "Expected an expression."))))
  (:documentation "Condition for when we expect an expression."))
