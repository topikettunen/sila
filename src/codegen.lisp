(in-package #:sila)

(defparameter *stack-depth* 0)

(defun asm-push ()
  (incf *stack-depth*)
  (format nil "push %rax"))

(defun asm-pop (reg)
  (assert (> *stack-depth* 0))
  (decf *stack-depth*)
  (format nil "pop %~a" reg))

(defmacro do-vector-push-inst (generator insts)
  `(loop :for inst :across ,generator :do (vector-push-extend inst ,insts)))

(defun make-inst-array ()
  (make-array 0 :adjustable t :fill-pointer 0))

(defparameter *label-count* 0
  "Global counter for to be used in ASM labels.")

(defun generate-statement (node)
  (let ((insts (make-inst-array)))
    (cond
      ((ast-node-block-p node)
       (loop :for body := (ast-node-block-body node)
               :then (setf body (next-node body))
             :until (null body)
             :do (do-vector-push-inst (generate-statement body) insts))
       insts)
      ((ast-node-return-p node)
       (do-vector-push-inst (generate-expression
                             (ast-node-return-expr node)) insts)
       (vector-push-extend "jmp .L.return" insts)
       insts)
      ((ast-node-break-p node)
       (vector-push-extend (format nil "jmp .L.end.~d"
                                   (ast-node-break-depth node)) insts)
       insts)
      ((ast-node-cond-p node)
       (incf *label-count*)
       (let ((count *label-count*))
         (do-vector-push-inst (generate-expression
                               (ast-node-cond-expr node)) insts)
         (vector-push-extend "cmp $0, %rax" insts)
         (vector-push-extend (format nil "je .L.else.~d" count) insts)
         (do-vector-push-inst (generate-statement
                               (ast-node-cond-then node)) insts)
         (vector-push-extend (format nil "jmp .L.end.~d" count) insts)
         (vector-push-extend (format nil ".L.else.~d:" count) insts)
         (if (ast-node-cond-else node)
             (do-vector-push-inst (generate-statement
                                   (ast-node-cond-else node)) insts)
             (vector-push-extend "nop" insts))
         (vector-push-extend (format nil ".L.end.~d:" count) insts)
         (unless (next-node node)
           (vector-push-extend "nop" insts)))
       insts)
      ((ast-node-for-p node)
       (incf *label-count*)
       (let ((count *label-count*))
         (do-vector-push-inst (generate-statement
                               (ast-node-for-init node)) insts)
         (vector-push-extend (format nil ".L.begin.~d:" count) insts)
         (when (ast-node-for-cond node)
           (do-vector-push-inst (generate-expression
                                 (ast-node-for-cond node)) insts)
           (vector-push-extend "cmp $0, %rax" insts)
           (vector-push-extend (format nil "je .L.end.~d" count) insts))
         (do-vector-push-inst (generate-statement
                               (ast-node-for-body node)) insts)
         (when (ast-node-for-inc node)
           (do-vector-push-inst (generate-expression
                                 (ast-node-for-inc node)) insts))
         (vector-push-extend (format nil "jmp .L.begin.~d" count) insts)
         (vector-push-extend (format nil ".L.end.~d:" count) insts)
         (unless (next-node node)
           (vector-push-extend "nop" insts)))
       insts)
      ((ast-node-loop-p node)
       (incf *label-count*)
       (let ((count *label-count*))
         (vector-push-extend (format nil ".L.begin.~d:" count) insts)
         (do-vector-push-inst (generate-statement
                               (ast-node-loop-body node)) insts)
         (vector-push-extend (format nil "jmp .L.begin.~d" count) insts)
         (vector-push-extend (format nil ".L.end.~d:" count) insts)
         (unless (next-node node)
           (vector-push-extend "nop" insts)))
       insts)
      ((ast-node-expression-p node)
       (do-vector-push-inst (generate-expression
                             (ast-node-expression-expr node)) insts)
       insts))))

(defun generate-expression (node)
  "Recursively generate the x86-64 assembly code."
  (flet ((lea (node)
           "Load effective address"
           (format nil "lea ~d(%rbp), %rax"
                   (object-offset
                    (ast-node-variable-object node)))))
    (let ((insts (make-inst-array)))
      (cond
        ((ast-node-integer-literal-p node)
         (vector-push-extend
          (format nil "mov $~d, %rax"
                  (ast-node-integer-literal-value node)) insts)
         insts)
        ((ast-node-negate-p node)
         (do-vector-push-inst (generate-expression
                               (ast-node-negate-value node)) insts)
         (vector-push-extend "neg %rax" insts)
         insts)
        ((ast-node-variable-p node)
         (vector-push-extend (lea node) insts)
         (vector-push-extend "mov (%rax), %rax" insts)
         insts)
        ((ast-node-assign-p node)
         (vector-push-extend (lea (ast-node-assign-var node)) insts)
         (vector-push-extend (asm-push) insts)
         (do-vector-push-inst (generate-expression
                               (ast-node-assign-expr node)) insts)
         (vector-push-extend (asm-pop "rdi") insts)
         (vector-push-extend "mov %rax, (%rdi)" insts)
         insts)
        ((ast-node-binop-p node)
         (generate-binop-expression node))))))

(defun generate-binop-expression (node)
  (let ((insts (make-inst-array)))
    (do-vector-push-inst (generate-expression
                          (ast-node-binop-rhs node)) insts)
    (vector-push-extend (asm-push) insts)
    (do-vector-push-inst (generate-expression
                          (ast-node-binop-lhs node)) insts)
    (vector-push-extend (asm-pop "rdi") insts)
    (let ((kind (ast-node-binop-kind node)))
      (ecase kind
        (:add
         (vector-push-extend "add %rdi, %rax" insts))
        (:sub
         (vector-push-extend "sub %rdi, %rax" insts))
        (:mul
         (vector-push-extend "imul %rdi, %rax" insts))
        (:div
         (vector-push-extend "cqo" insts)
         (vector-push-extend "idiv %rdi, %rax" insts))
        ((:equal
          :not-equal
          :lesser-than
          :lesser-or-equal
          :greater-than
          :greater-or-equal)
         (vector-push-extend "cmp %rdi, %rax" insts)
         (case kind
           (:equal
            (vector-push-extend "sete %al" insts))
           (:not-equal
            (vector-push-extend "setne %al" insts))
           (:lesser-than
            (vector-push-extend "setl %al" insts))
           (:lesser-or-equal
            (vector-push-extend "setle %al" insts))
           (:greater-than
            (vector-push-extend "setg %al" insts))
           (:greater-or-equal
            (vector-push-extend "setge %al" insts))
           (otherwise (values)))
         (vector-push-extend "movzb %al, %rax" insts))))
    insts))

(defun emit-code (src &key (stream nil) (indent 2) (indent-tabs t))
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
    (let ((program (parse-program (tokenize src))))
      ;; TODO(topi): these instructions probably should be collected to some
      ;; structure so they can be divided in to sections more easily when the
      ;; programs become more complex.
      (format stream
              "~{~a~%~}"
              (alexandria:flatten
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
                (loop :for inst
                        :across (generate-statement (func-body program))
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
                (format nil "~aret" indent)))))))
