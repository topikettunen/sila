(defpackage #:sila
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind)
  (:import-from #:sila/parser
                #:parse-expression-node)
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
    ;; TODO(topi): Don't really like this `asm-conc' shenanigans. Refactor it.
    (flet ((asm-conc (inst)
             (setf asm (concatenate 'string asm inst))))
      (multiple-value-bind (node rest)
          (parse-expression-node (tokenize src))
        (unless (eq (token-kind rest) :eof)
          (error 'parser-error :error-msg "Extra tokens"))
        (asm-conc (asm-directive ".globl main"))
        (asm-conc (asm-label "main"))
        (asm-conc (generate-expr node))
        (asm-conc (asm-inst "ret"))))
    asm))
