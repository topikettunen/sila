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
  (let ((asm "")
        (kind (ast-node-kind node)))
    ;; TODO(topi): Don't really like this `asm-conc' shenanigans. Refactor it.
    (flet ((asm-conc (inst)
             (setf asm (concatenate 'string asm inst))))
      (cond ((eq kind :number)
             (asm-conc (asm-inst (format nil "mov $~d, %rax"
                                         (ast-node-val node))))
             (return-from generate-expr asm))
            ((eq kind :neg)
             (asm-conc (generate-expr (ast-node-lhs node)))
             (asm-conc (asm-inst "neg %rax"))
             (return-from generate-expr asm)))

      (asm-conc (generate-expr (ast-node-rhs node)))
      (asm-conc (asm-push))
      (asm-conc (generate-expr (ast-node-lhs node)))
      (asm-conc (asm-pop "rdi"))

      (cond ((eq kind :add)
             (asm-conc (asm-inst "add %rdi, %rax")))

            ((eq kind :sub)
             (asm-conc (asm-inst "sub %rdi, %rax")))

            ((eq kind :mul)
             (asm-conc (asm-inst "imul %rdi, %rax")))

            ((eq kind :div)
             (asm-conc (asm-inst "cqo"))
             (asm-conc (asm-inst "idiv %rdi, %rax")))

            ((or (eq kind :equal)
                 (eq kind :not-equal)
                 (eq kind :lesser-than)
                 (eq kind :lesser-or-equal)
                 (eq kind :greater-than)
                 (eq kind :greater-or-equal))
             (asm-conc (asm-inst "cmp %rdi, %rax"))
             (cond ((eq kind :equal)
                    (asm-conc (asm-inst "sete %al")))

                   ((eq kind :not-equal)
                    (asm-conc (asm-inst "setne %al")))

                   ((eq kind :lesser-than)
                    (asm-conc (asm-inst "setl %al")))

                   ((eq kind :lesser-or-equal)
                    (asm-conc (asm-inst "setle %al")))

                   ((eq kind :greater-than)
                    (asm-conc (asm-inst "setg %al")))

                   ((eq kind :greater-or-equal)
                    (asm-conc (asm-inst "setge %al"))))
             (asm-conc (asm-inst "movzb %al, %rax")))
            (t (error 'parser-error))))
    asm))
