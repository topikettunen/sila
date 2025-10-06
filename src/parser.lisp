(in-package #:sila)

(defstruct (ast-node
            (:copier nil))
  (next nil :type t))

(defgeneric next-node (node))

(defstruct (ast-node-block
            (:include ast-node)
            (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-block))
  (ast-node-block-next node))

(defstruct (ast-node-negate
            (:include ast-node)
            (:copier nil))
  (value (util:required-argument 'value) :type t :read-only t))

(defmethod next-node ((node ast-node-negate))
  (ast-node-negate-next node))

(defstruct (ast-node-integer-literal
            (:include ast-node)
            (:copier nil))
  (value (util:required-argument 'value) :type integer :read-only t))

(defmethod next-node ((node ast-node-integer-literal))
  (ast-node-integer-literal-next node))

(defstruct (ast-node-variable
            (:include ast-node)
            (:copier nil))
  (object (util:required-argument 'object) :type object :read-only t))

(defmethod next-node ((node ast-node-variable))
  (ast-node-variable-next node))

(defstruct (ast-node-return
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-return))
  (ast-node-return-next node))

(defstruct (ast-node-assign
            (:include ast-node)
            (:copier nil))
  (var (util:required-argument 'var) :type ast-node-variable :read-only t)
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-assign))
  (ast-node-assign-next node))

(defstruct (ast-node-cond
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t)
  (then (util:required-argument 'then) :type t :read-only t)
  (else (util:required-argument 'else) :type t :read-only t))

(defmethod next-node ((node ast-node-cond))
  (ast-node-cond-next node))

(deftype binop-kind ()
  '(member
    :add
    :sub
    :mul
    :div
    :equal
    :not-equal
    :lesser-than
    :lesser-or-equal
    :greater-than
    :greater-or-equal))

(defstruct (ast-node-binop
            (:include ast-node)
            (:copier nil))
  (kind (util:required-argument 'kind) :type binop-kind :read-only t)
  (lhs (util:required-argument 'lhs) :type t :read-only t)
  (rhs (util:required-argument 'rhs) :type t :read-only t))

(defmethod next-node ((node ast-node-binop))
  (ast-node-binop-next node))

;;; TODO(topi): maybe this struct is slightly too general?
(defstruct (ast-node-expression
            (:include ast-node)
            (:copier nil))
  (expr (util:required-argument 'expr) :type t :read-only t))

(defmethod next-node ((node ast-node-expression))
  (ast-node-expression-next node))

(defstruct (ast-node-for
             (:include ast-node)
             (:copier nil))
  (init (util:required-argument 'init) :type t :read-only t)
  (inc (util:required-argument 'inc) :type t :read-only t)
  (cond (util:required-argument 'cond) :type t :read-only t)
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-for))
  (ast-node-for-next node))

(defstruct (ast-node-loop
             (:include ast-node)
             (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t))

(defmethod next-node ((node ast-node-loop))
  (ast-node-loop-next node))

(defstruct (ast-node-break
             (:include ast-node)
             (:copier nil))
  (depth (util:required-argument 'depth) :type integer :read-only t))

(defmethod next-node ((node ast-node-break))
  (ast-node-break-next node))

(defvar *local-variables* nil
  "Global variable for holding local variable objects.")

(defstruct (object
            (:copier nil))
  (name (util:required-argument 'name) :type string :read-only t)
  (offset 0 :type integer)
  (next nil :type t))

(defstruct (func
            (:copier nil))
  (body (util:required-argument 'body) :type t :read-only t)
  (locals (util:required-argument 'locals) :type t :read-only t)
  (stack-size 0 :type integer))

(defun parse-statement-node (tokens)
  (alexandria:switch ((token-literal (first tokens)) :test #'string=)
    ("return"
     (multiple-value-bind (node rest)
         (parse-expression-node (rest tokens))
       (values (make-ast-node-return :expr node)
               (rest (skip-to-token ";" rest)))))
    ("if"
     (parse-cond-statement-node (rest tokens)))
    ("for"
     (parse-for-statement-node (rest tokens)))
    ("loop"
     (parse-loop-statement-node (rest tokens)))
    ("break"
     (values (make-ast-node-break :depth *break-depth*)
             (rest (skip-to-token ";" tokens))))
    ("{"
     (parse-compound-statement-node (rest tokens)))
    (otherwise
     (parse-expression-statement-node tokens))))

(defun parse-cond-statement-node (tokens)
  ;; TOK passed in should be the token after "if".
  (let (expr then else)
    (multiple-value-bind (expr-node rest)
        (parse-expression-node tokens)
      (setf expr expr-node
            tokens rest))
    (multiple-value-bind (then-node rest)
        (parse-statement-node tokens)
      (setf then then-node
            tokens rest))
    (when (string= (token-literal (first tokens)) "else")
      (multiple-value-bind (else-node rest)
          (parse-statement-node (rest tokens))
        (setf else else-node
              tokens rest)))
    (values (make-ast-node-cond :expr expr
                                :then then
                                :else else)
            tokens)))

(defvar *break-depth* 0
  "Depth counter for BREAK keyword to know from what level it should break out.")

(defun parse-for-statement-node (tokens)
  ;; TOK passed in should be the token after "for"
  (let (init cond inc body)
    (multiple-value-bind (init-node rest)
        (parse-expression-statement-node tokens)
      (setf init init-node
            tokens rest))
    (unless (string= (token-literal (first tokens)) ";")
      (multiple-value-bind (cond-node rest)
          (parse-expression-node tokens)
        (setf cond cond-node
              tokens rest)))
    (setf tokens (skip-to-token ";" tokens))
    ;; Entering "for" scope.
    (incf *break-depth*)
    (unless (string= (token-literal (second tokens)) "{")
      (multiple-value-bind (inc-node rest)
          (parse-expression-node (rest tokens))
        (setf inc inc-node
              tokens rest)))
    (setf tokens (skip-to-token "{" tokens))
    (multiple-value-bind (body-node rest)
        (parse-statement-node tokens)
      (setf body body-node
            tokens rest))
    ;; Left "for" scope.
    (decf *break-depth*)
    (values (make-ast-node-for :init init
                               :cond cond
                               :inc inc
                               :body body)
            tokens)))

(defun parse-loop-statement-node (tokens)
  ;; TOK passed in should be the opening brace of the block after the "loop"
  ;; keyword.
  ;; TODO(topi): Add proper error handling.
  (assert (string= (token-literal (first tokens)) "{"))
  ;; Entering "loop" scope.
  (incf *break-depth*)
  (multiple-value-bind (body rest)
      (parse-statement-node tokens)
    ;; Left "loop" scope.
    (decf *break-depth*)
    (values (make-ast-node-loop :body body)
            rest)))

(defun parse-compound-statement-node (tokens)
  (let* ((head (make-ast-node))
         (cur head))
    (loop :until (string= (token-literal (first tokens)) "}")
          :do (multiple-value-bind (node rest)
                  (parse-statement-node tokens)
                (setf (ast-node-next cur) node)
                (setf cur (ast-node-next cur))
                (setf tokens rest)))
    (values (make-ast-node-block :body (ast-node-next head))
            (rest tokens))))

(defun parse-expression-statement-node (tokens)
  ;; Empty statement, e.g. ';;; return 3;'
  (when (string= (token-literal (first tokens)) ";")
    (return-from parse-expression-statement-node
      (values (make-ast-node-block :body nil)
              (rest tokens))))
  (multiple-value-bind (node rest)
      (parse-expression-node tokens)
    (values (make-ast-node-expression :expr node)
            (rest (skip-to-token ";" rest)))))

(defun parse-expression-node (tokens)
  (parse-assign-node tokens))

(defun parse-assign-node (tokens)
  (let (node)
    (multiple-value-bind (eql-node rest)
        (parse-equality-node tokens)
      (setf node eql-node
            tokens rest))
    (when (string= (token-literal (first tokens)) ":=")
      (multiple-value-bind (expr-node rest)
          (parse-assign-node (rest tokens))
        (setf node (make-ast-node-assign :var node
                                         :expr expr-node)
              tokens rest)))
    (values node tokens)))

(defmacro define-binop-parser (name &key descent-parser comparison-symbols)
  "Macro for generating new binary operator parser rules."
  (let ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
        (descent-parser-name (intern (format nil "PARSE-~a-NODE" descent-parser))))
    `(defun ,parser-name (tokens)
       (let (node)
         (multiple-value-bind (lhs rest)
             (,descent-parser-name tokens)
           (setf node lhs)
           (loop
            (cond
              ,@(loop :for symbol :in comparison-symbols
                      :collect `((string= (token-literal (first rest)) ,(car symbol))
                                 (multiple-value-bind (rhs rest2)
                                     (,descent-parser-name (rest rest))
                                   (setf node (make-ast-node-binop
                                               :kind ,(cdr symbol)
                                               :lhs node
                                               :rhs rhs))
                                   (setf rest rest2))))
              (t
               (return-from ,parser-name
                 (values node rest))))))))))

(define-binop-parser equality
  :descent-parser relational
  :comparison-symbols (("==" . :equal)
                       ("!=" . :not-equal)))

(define-binop-parser relational
  :descent-parser add
  :comparison-symbols (("<" . :lesser-than)
                       ("<=" . :lesser-or-equal)
                       (">" . :greater-than)
                       (">=" . :greater-or-equal)))

(define-binop-parser add
  :descent-parser multiplicative
  :comparison-symbols (("+" . :add)
                       ("-" . :sub)))

(define-binop-parser multiplicative
  :descent-parser unary
  :comparison-symbols (("*" . :mul)
                       ("/" . :div)))

(defun parse-unary-node (tokens)
  (alexandria:switch ((token-literal (first tokens)) :test #'string=)
    ("+"
     (parse-unary-node (rest tokens)))
    ("-"
     (multiple-value-bind (node rest)
         (parse-unary-node (rest tokens))
       (values (make-ast-node-negate :value node)
               rest)))
    (t
     (parse-primary-node tokens))))

(defun parse-primary-node (tokens)
  (flet ((find-local-var (name)
           (loop :for obj := *local-variables*
                   :then (setf obj (object-next obj))
                 :until (null obj)
                 :do (when (string= name (object-name obj))
                       (return-from find-local-var obj)))))
    (cond
      ((eq (token-kind (first tokens)) :num)
       (values (make-ast-node-integer-literal
                :value (parse-integer (token-literal (first tokens))))
               (rest tokens)))
      ((eq (token-kind (first tokens)) :ident)
       (let* ((name (token-literal (first tokens)))
              (var (find-local-var name)))
         (when (null var)
           (setf var (make-object :name name :next *local-variables*))
           ;; New object should be in front of the list.
           (setf *local-variables* var))
         (values (make-ast-node-variable :object var)
                 (rest tokens))))
      ((string= (token-literal (first tokens)) "(")
       (multiple-value-bind (node rest)
           (parse-expression-node (rest tokens))
         (values node (rest (skip-to-token ")" rest)))))
      (t (error "Unexpected token value: ~a" tokens)))))

(defun parse-program (tokens)
  (labels ((align-to (n align)
             "Round N to the nearest multiple of ALIGN."
             (* (ceiling n align) align))
           (set-lvar-offsets (program)
             (let ((offset 0))
               (loop :for obj := (func-locals program)
                       :then (setf obj (object-next obj))
                     :until (null obj)
                     :do (progn
                           (incf offset 8)
                           (setf (object-offset obj) (- offset))))
               (setf (func-stack-size program) (align-to offset 16)))
             (values)))
    (let* ((toks tokens)
           (head (make-ast-node))
           (cur head))
      (loop :until (eq (token-kind (first toks)) :eof)
            :do (multiple-value-bind (node rest)
                    (parse-statement-node toks)
                  (setf (ast-node-next cur) node)
                  (setf cur (ast-node-next cur))
                  (setf toks rest)))
      (let ((program (make-func :body (ast-node-next head)
                                :locals *local-variables*)))
        (set-lvar-offsets program)
        program))))

;;;
;;; Parser Utilities
;;;

(defun skip-to-token (val tokens)
  "Search for token which is VAL. Returns NIL when not found."
  (let ((toks tokens))
    (loop :until (eq (token-kind (first toks)) :eof)
          :do (if (string= (token-literal (first toks)) val)
                  (return-from skip-to-token toks)
                  (setf toks (rest toks))))))
