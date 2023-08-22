(defpackage #:sila
  (:use #:cl)
  (:import-from #:sila/conditions
                #:lexer-error
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:import-from #:sila/parser
                #:expr)
  (:import-from #:sila/codegen
                #:asm-directive
                #:asm-label
                #:asm-inst
                #:generate-expr)
  (:export #:emit-asm))
(in-package #:sila)

(defun emit-asm (src)
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  (let ((asm ""))
    (flet ((asm-conc (inst)
             (setf asm (concatenate 'string asm inst))))
      (multiple-value-bind (node rest)
          (expr (tokenize src))
        (unless (eq (token-kind rest) :eof)
          (error 'parser-error :error-msg "Extra tokens"))
        (asm-conc (asm-directive ".globl main"))
        (asm-conc (asm-label "main"))
        (asm-conc (generate-expr node))
        (asm-conc (asm-inst "ret"))))
    asm))
