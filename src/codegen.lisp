(defpackage #:sila/codegen
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf
                #:flatten)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind)
  (:import-from #:sila/parser
                #:ast-node-body
                #:ast-node-kind
                #:ast-node-value
                #:ast-node-variable
                #:ast-node-lhs
                #:ast-node-rhs
                #:ast-node-next
                #:object-offset
                #:func-body
                #:func-stack-size
                #:parse-program)
  (:export #:emit-code))
(in-package #:sila/codegen)

(defparameter *stack-depth* 0)

(defun asm-push ()
  (incf *stack-depth*)
  (format nil "push %rax"))

(defun asm-pop (reg)
  (assert (> *stack-depth* 0))
  (decf *stack-depth*)
  (format nil "pop %~a" reg))

(defmacro do-vector-push-inst (generator insts)
  `(loop for inst across ,generator do (vector-push-extend inst ,insts)))

(defun make-inst-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defun generate-statement (node)
  (let ((insts (make-inst-array)))
    (ecase (ast-node-kind node)
      (:compound-statement
       (loop for body = (ast-node-body node)
               then (setf body (ast-node-next body))
             until (null body)
             do (do-vector-push-inst (generate-statement body) insts)))
      (:return-statement
       (do-vector-push-inst (generate-code (ast-node-lhs node)) insts)
       (vector-push-extend (format nil "jmp .L.return") insts))
      (:expression-statement
       (do-vector-push-inst (generate-code (ast-node-lhs node)) insts)))
    insts))

(defun generate-address (node)
  (if (eql (ast-node-kind node) :variable)
      (format nil "lea ~a(%rbp), %rax" (object-offset (ast-node-variable node)))
      (error "Expected lvalue, got: ~a" node)))

(defun generate-code (node)
  "Recursively generate the x86-64 assembly code."
  (let ((kind (ast-node-kind node))
        (insts (make-inst-array)))
    (case kind
      (:number
       (vector-push-extend (format nil "mov $~d, %rax" (ast-node-value node)) insts)
       (return-from generate-code insts))
      (:neg
       (do-vector-push-inst (generate-code (ast-node-lhs node)) insts)
       (vector-push-extend (format nil "neg %rax") insts)
       (return-from generate-code insts))
      (:variable
       (vector-push-extend (generate-address node) insts)
       (vector-push-extend (format nil "mov (%rax), %rax") insts)
       (return-from generate-code insts))
      (:assign
       (vector-push-extend (generate-address (ast-node-lhs node)) insts)
       (vector-push-extend (asm-push) insts)
       (do-vector-push-inst (generate-code (ast-node-rhs node)) insts)
       (vector-push-extend (asm-pop "rdi") insts)
       (vector-push-extend (format nil "mov %rax, (%rdi)") insts)
       (return-from generate-code insts))
      (otherwise (values)))
    (do-vector-push-inst (generate-code (ast-node-rhs node)) insts)
    (vector-push-extend (asm-push) insts)
    (do-vector-push-inst (generate-code (ast-node-lhs node)) insts)
    (vector-push-extend (asm-pop "rdi") insts)
    (ecase kind
      (:add
       (vector-push-extend (format nil "add %rdi, %rax") insts))
      (:sub
       (vector-push-extend (format nil "sub %rdi, %rax") insts))
      (:mul
       (vector-push-extend (format nil "imul %rdi, %rax") insts))
      (:div
       (vector-push-extend (format nil "cqo") insts)
       (vector-push-extend (format nil "idiv %rdi, %rax") insts))
      ((:equal
        :not-equal
        :lesser-than
        :lesser-or-equal
        :greater-than
        :greater-or-equal)
       (vector-push-extend (format nil "cmp %rdi, %rax") insts)
       (case kind
         (:equal
          (vector-push-extend (format nil "sete %al") insts))
         (:not-equal
          (vector-push-extend (format nil "setne %al") insts))
         (:lesser-than
          (vector-push-extend (format nil "setl %al") insts))
         (:lesser-or-equal
          (vector-push-extend (format nil "setle %al") insts))
         (:greater-than
          (vector-push-extend (format nil "setg %al") insts))
         (:greater-or-equal
          (vector-push-extend (format nil "setge %al") insts))
         (otherwise (values)))
       (vector-push-extend (format nil "movzb %al, %rax") insts)))
    insts))

(defun emit-code (src &key (stream nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  ;; Init environment.
  ;; TODO(topi): These should probably be set in some smarter place.
  (setf sila/parser::*local-variables* nil
        sila/codegen::*stack-depth* 0)
  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))
    (let ((program (parse-program (tokenize src))))
      ;; TODO(topi): these instructions probably should be collected to some
      ;; structure so they can be divided in to sections more easily when the
      ;; programs become more complex.
      (format stream
              "~{~a~%~}"
              (flatten
               (list
                ;; ASM Directive
                (format nil "~a.globl main" indent)
                ;; Main Label
                "main:"
                ;; Prologue
                (format nil "~apush %rbp" indent)
                (format nil "~amov %rsp, %rbp" indent)
                (format nil "~asub $~a, %rsp" indent (func-stack-size program))
                ;; ASM Routine
                (loop for inst across (generate-statement (func-body program))
                      collect (format nil "~a~a" indent inst))
                ;; Return label
                ".L.return:"
                ;; Epilogue
                (format nil "~amov %rbp, %rsp" indent)
                (format nil "~apop %rbp" indent)
                ;; Return
                (format nil "~aret" indent)))))))
