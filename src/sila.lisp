(defpackage #:sila
  (:use #:cl)
  (:import-from #:alexandria
                #:flatten)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind)
  (:import-from #:sila/parser
                #:parse-expression-node)
  (:import-from #:sila/codegen
                #:generate-expr)
  (:export #:emit-asm))
(in-package #:sila)

(defun emit-asm (src &key (print-to-stdout nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))
    (multiple-value-bind (node tokens)
        (parse-expression-node (tokenize src))
      (unless (eq (token-kind (first tokens)) :eof)
        (error 'parser-error :error-msg "Extra tokens"))
      (format print-to-stdout
              "~{~a~%~}"
              (flatten
               (list
                ;; ASM Directive
                (format nil "~a.globl main" indent)
                ;; ASM Label
                "main:"
                ;; ASM Routine
                (loop :for inst :in (generate-expr node)
                      :collect (format nil "~a~a" indent inst))
                ;; Return
                (format nil "~aret" indent)))))))
