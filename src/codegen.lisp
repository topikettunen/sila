(in-package #:sila)

(defparameter *stack-depth* 0)

(defun asm-push ()
  (incf *stack-depth*)
  (format nil "push %rax"))

(defun asm-pop (reg)
  (assert (> *stack-depth* 0))
  (decf *stack-depth*)
  (format nil "pop %~a" reg))

(defmacro push-inst (str insts)
  `(vector-push-extend ,str ,insts))

(defmacro do-push-inst (generator insts)
  `(loop :for inst :across ,generator :do (push-inst inst ,insts)))

(defun make-inst-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defparameter *label-count* 0
  "Global counter for to be used in ASM labels.")

(defun generate-statement (nodes)
  (let ((insts (make-inst-array))
        (cur-node (if (listp nodes)
                      (first nodes)
                      nodes)))
    (cond
      ((ast-node-block-p cur-node)
       (loop :for body := (ast-node-block-body cur-node)
               :then (setf body (rest body))
             :until (null body)
             :do (do-push-inst (generate-statement body) insts))
       insts)
      ((ast-node-return-p cur-node)
       (do-push-inst (generate-expression
                      (ast-node-return-expr cur-node)) insts)
       (push-inst "jmp .L.return" insts)
       insts)
      ((ast-node-break-p cur-node)
       (push-inst (format nil "jmp .L.end.~d"
                          (ast-node-break-depth cur-node)) insts)
       insts)
      ((ast-node-cond-p cur-node)
       (incf *label-count*)
       (let ((count *label-count*))
         (do-push-inst (generate-expression
                        (ast-node-cond-expr cur-node)) insts)
         (push-inst "cmp $0, %rax" insts)
         (push-inst (format nil "je .L.else.~d" count) insts)
         (do-push-inst (generate-statement
                        (ast-node-cond-then cur-node)) insts)
         (push-inst (format nil "jmp .L.end.~d" count) insts)
         (push-inst (format nil ".L.else.~d:" count) insts)
         (if (ast-node-cond-else cur-node)
             (do-push-inst (generate-statement
                            (ast-node-cond-else cur-node)) insts)
             (push-inst "nop" insts))
         (push-inst (format nil ".L.end.~d:" count) insts)
         (unless (rest nodes)
           (push-inst "nop" insts)))
       insts)
      ((ast-node-for-p cur-node)
       (incf *label-count*)
       (let ((count *label-count*))
         (do-push-inst (generate-statement
                        (ast-node-for-init cur-node)) insts)
         (push-inst (format nil ".L.begin.~d:" count) insts)
         (when (ast-node-for-cond cur-node)
           (do-push-inst (generate-expression
                          (ast-node-for-cond cur-node)) insts)
           (push-inst "cmp $0, %rax" insts)
           (push-inst (format nil "je .L.end.~d" count) insts))
         (do-push-inst (generate-statement
                        (ast-node-for-body cur-node)) insts)
         (when (ast-node-for-inc cur-node)
           (do-push-inst (generate-expression
                          (ast-node-for-inc cur-node)) insts))
         (push-inst (format nil "jmp .L.begin.~d" count) insts)
         (push-inst (format nil ".L.end.~d:" count) insts)
         (unless (rest nodes)
           (push-inst "nop" insts)))
       insts)
      ((ast-node-loop-p cur-node)
       (incf *label-count*)
       (let ((count *label-count*))
         (push-inst (format nil ".L.begin.~d:" count) insts)
         (do-push-inst (generate-statement
                        (ast-node-loop-body cur-node)) insts)
         (push-inst (format nil "jmp .L.begin.~d" count) insts)
         (push-inst (format nil ".L.end.~d:" count) insts)
         (unless (rest nodes)
           (push-inst "nop" insts)))
       insts)
      ((ast-node-expression-p cur-node)
       (do-push-inst (generate-expression
                      (ast-node-expression-expr cur-node)) insts)
       insts))))

(defun generate-expression (node)
  (flet ((lea (node)
           "Load effective address"
           (format nil "lea ~d(%rbp), %rax"
                   (object-offset
                    (ast-node-variable-object node)))))
    (let ((insts (make-inst-array)))
      (cond
        ((ast-node-integer-literal-p node)
         (push-inst
          (format nil "mov $~d, %rax"
                  (ast-node-integer-literal-value node)) insts)
         insts)
        ((ast-node-negate-p node)
         (do-push-inst (generate-expression
                        (ast-node-negate-value node)) insts)
         (push-inst "neg %rax" insts)
         insts)
        ((ast-node-variable-p node)
         (push-inst (lea node) insts)
         (push-inst "mov (%rax), %rax" insts)
         insts)
        ((ast-node-assign-p node)
         (push-inst (lea (ast-node-assign-var node)) insts)
         (push-inst (asm-push) insts)
         (do-push-inst (generate-expression
                        (ast-node-assign-expr node)) insts)
         (push-inst (asm-pop "rdi") insts)
         (push-inst "mov %rax, (%rdi)" insts)
         insts)
        ((ast-node-binop-p node)
         (generate-binop-expression node))))))

(defun generate-binop-expression (node)
  (let ((insts (make-inst-array)))
    (do-push-inst (generate-expression
                   (ast-node-binop-rhs node)) insts)
    (push-inst (asm-push) insts)
    (do-push-inst (generate-expression
                   (ast-node-binop-lhs node)) insts)
    (push-inst (asm-pop "rdi") insts)
    (let ((kind (ast-node-binop-kind node)))
      (ecase kind
        (:add
         (push-inst "add %rdi, %rax" insts))
        (:sub
         (push-inst "sub %rdi, %rax" insts))
        (:mul
         (push-inst "imul %rdi, %rax" insts))
        (:div
         (push-inst "cqo" insts)
         (push-inst "idiv %rdi, %rax" insts))
        ((:equal
          :not-equal
          :lesser-than
          :lesser-or-equal
          :greater-than
          :greater-or-equal)
         (push-inst "cmp %rdi, %rax" insts)
         (case kind
           (:equal
            (push-inst "sete %al" insts))
           (:not-equal
            (push-inst "setne %al" insts))
           (:lesser-than
            (push-inst "setl %al" insts))
           (:lesser-or-equal
            (push-inst "setle %al" insts))
           (:greater-than
            (push-inst "setg %al" insts))
           (:greater-or-equal
            (push-inst "setge %al" insts))
           (otherwise (values)))
         (push-inst "movzb %al, %rax" insts))))
    insts))

(defun emit-code (program &key (stream nil) (indent 2) (indent-tabs t))
  "Emit assembly code from given source code. Currently emits only x86-64 and
only Linux is tested."
  ;; Init environment
  (setf *local-variables* nil
        *stack-depth* 0
        *label-count* 0)
  (let ((indent (if indent-tabs
                    #\Tab
                    (coerce (make-list indent
                                       :initial-element #\Space)
                            'string))))
    ;; TODO(topi): these instructions probably should be collected to some
    ;; structure so they can be divided in to sections more easily when the
    ;; programs become more complex.
    (format stream
            "~{~a~%~}"
            (util:flatten
             (list
              ;; ASM Directive
              (format nil "~a.globl main" indent)
              ;; Main Label
              "main:"
              ;; Prologue
              (format nil "~apush %rbp" indent)
              (format nil "~amov %rsp, %rbp" indent)
              (format nil "~asub $~a, %rsp" indent
                      (func-stack-size program))
              ;; ASM Routine
              (loop :for inst :across (generate-statement (func-body program))
                    :collect (if (string= (subseq inst 0 3) ".L.")
                                 ;; If instruction is label (.L. prefix),
                                 ;; don't indent it.
                                 inst
                                 (format nil "~a~a" indent inst)))
              ;; Return label
              ".L.return:"
              ;; Epilogue
              (format nil "~amov %rbp, %rsp" indent)
              (format nil "~apop %rbp" indent)
              ;; Return
              (format nil "~aret" indent))))))
