(defpackage #:sila/codegen
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/parser
                #:ast-node-kind
                #:ast-node-val
                #:ast-node-lhs
                #:ast-node-rhs)
  (:export #:asm-inst
           #:asm-directive
           #:asm-label
           #:generate-expr))
(in-package #:sila/codegen)

(defun asm-inst (inst)
  (format nil "  ~a~%" inst))

(defun asm-directive (dir)
  (asm-inst dir))

(defun asm-label (label)
  (format nil "~a:~%" label))

(defun asm-push ()
  (asm-inst "push %rax"))

(defun asm-pop (reg)
  (asm-inst (format nil "pop %~a" reg)))

(defun generate-expr (node)
  "Recursively generate the x86 assembly code."
  (let ((asm ""))
    (flet ((asm-conc (inst)
             (setf asm (concatenate 'string asm inst))))
      (alexandria:switch ((ast-node-kind node) :test #'eq)
        (:number (asm-conc (asm-inst (format nil "mov $~d, %rax"
                                             (ast-node-val node))))
                 (return-from generate-expr asm))
        (:neg (asm-conc (generate-expr (ast-node-lhs node)))
              (asm-conc (asm-inst "neg %rax"))
              (return-from generate-expr asm)))
      (asm-conc (generate-expr (ast-node-rhs node)))
      (asm-conc (asm-push))
      (asm-conc (generate-expr (ast-node-lhs node)))
      (asm-conc (asm-pop "rdi"))
      (alexandria:switch ((ast-node-kind node) :test #'eq)
        (:add (asm-conc (asm-inst "add %rdi, %rax")))
        (:sub (asm-conc (asm-inst "sub %rdi, %rax")))
        (:mul (asm-conc (asm-inst "imul %rdi, %rax")))
        (:div (asm-conc (asm-inst "cqo"))
              (asm-conc (asm-inst "idiv %rdi, %rax")))
        (t (error 'parser-error))))
    asm))
