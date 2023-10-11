(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:token-kind
                #:token-value))
(in-package #:sila/parser)

(deftype ast-node-kind ()
  "Sila AST node kind."
  '(member
    :add
    :sub
    :mul
    :div
    :neg
    :equal
    :not-equal
    :lesser-than
    :lesser-or-equal
    :greater-than
    :greater-or-equal
    :assign
    :expression-statement
    :variable
    :number))

(defstruct ast-node
  "Structure for Sila AST nodes."
  kind
  value    ; Used if `kind' is `:number'.
  variable ; Used if `kind' is `:variable'.
  lhs
  rhs)

(defparameter *local-variables* '()
  "Local variable instances created during parsing.")

(defstruct object
  "Structure for local variables."
  name
  offset)

(defstruct func
  "Structure for functions."
  body
  locals
  stack-size)

(defun skip-to-token (val tok)
  "Search for token which is `val'. Returns `nil' when not found.

TODO(topi): Probably should do some proper error handling if `val' isn't
found."
  (cond ((eql (token-kind (first tok)) :eof) nil)
        ((string= (token-value (first tok)) val) tok)
        (t (skip-to-token val (rest tok)))))

(defun find-local-var (name)
  (dolist (var *local-variables*)
    (when (string= name (object-name var))
      (return-from find-local-var var))))

(defmacro define-parser (name &key descent-parser comparison-symbols bnf)
  "Macro for generating new parser rules."
  (let ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
        (descent-parser-name (intern (format nil "PARSE-~a-NODE" descent-parser))))
    `(defun ,parser-name (tok)
       ,bnf
       (multiple-value-bind (node tokens)
           (,descent-parser-name tok)
         (loop
           (cond
             ,@(loop :for symbol :in comparison-symbols
                     :collect `((string= (token-value (first tokens)) ,(car symbol))
                                (multiple-value-bind (node2 tokens2)
                                    (,descent-parser-name (rest tokens))
                                  (setf node (make-ast-node :kind ,(cdr symbol)
                                                            :lhs node
                                                            :rhs node2))
                                  (setf tokens tokens2))))
             (t
              (return-from ,parser-name
                (values node tokens)))))))))

;;; These parsing rules are just defined by singular other parsing rule, so
;;; for now, it doesn't require using macro for defining the rule.

(defun parse-statement-node (tok)
  "statement-node ::== expression-statement-node"
  (parse-expression-statement-node tok))

;;; TODO(topi): Could the `define-parser' macro be improved to handle this
;;; rule?
(defun parse-expression-statement-node (tok)
  "expression-statement-node ::== expression-node ';'"
  (multiple-value-bind (node tokens)
      (parse-expression-node tok)
    (values (make-ast-node :kind :expression-statement
                           :lhs node)
            (rest (skip-to-token ";" tokens)))))

(defun parse-expression-node (tok)
  "expression-node ::== assign"
  (parse-assign-node tok))

;;; TODO(topi): Could the `define-parser' macro be improved to handle this
;;; rule?
(defun parse-assign-node (tok)
  "assign-node ::== equality ( '<-' assign ) ?"
  (multiple-value-bind (node tokens)
      (parse-equality-node tok)
    (when (string= (token-value (first tokens)) "<-")
      (multiple-value-bind (node2 tokens2)
          (parse-assign-node (rest tokens))
        (setf node (make-ast-node :kind :assign
                                  :lhs node
                                  :rhs node2))
        (setf tokens tokens2)))
    (values node tokens)))

(define-parser equality
  :descent-parser relational
  :comparison-symbols (("==" . :equal)
                       ("!=" . :not-equal))
  :bnf "equality-node ::== relational-node ( '==' relational-node | '!=' relational-node ) *")

(define-parser relational
  :descent-parser add
  :comparison-symbols (("<" . :lesser-than)
                       ("<=" . :lesser-or-equal)
                       (">" . :greater-than)
                       (">=" . :greater-or-equal))
  :bnf "relational-node ::== add ( '<'  add | '<=' add | '>'  add | '>=' add ) *")

(define-parser add
  :descent-parser multiplicative
  :comparison-symbols (("+" . :add)
                       ("-" . :sub))
  :bnf "add-node ::== multiplicative-node ( '+' multiplicative-node | '-' multiplicative-node ) *")

(define-parser multiplicative
  :descent-parser unary
  :comparison-symbols (("*" . :mul)
                       ("/" . :div))
  :bnf "multiplicative-node ::== unary-node ( '*' unary-node | '/' unary-node ) *")

;;; Since parsing unary operators and primary nodes works slightly
;;; differently, I'll just write these functions by hand.

(defun parse-unary-node (tok)
  "unary-node ::== ( '+' | '-' ) unary | primary-node"
  (cond ((string= (token-value (first tok)) "+")
         (parse-unary-node (rest tok)))
        ((string= (token-value (first tok)) "-")
         (multiple-value-bind (node tokens)
             (parse-unary-node (rest tok))
           ;; In case something like '--10' is encountered.
           (when (eql (token-value (first tokens)) #\-)
             (parse-unary-node (rest tokens)))
           (values (make-ast-node :kind :neg :lhs node)
                   tokens)))
        (t
         (parse-primary-node tok))))

(defun parse-primary-node (tok)
  "primary-node ::== '(' expression-node ')' | ident | number"
  (cond ((eql (token-kind (first tok)) :num)
         (values (make-ast-node :kind :number :value (token-value (first tok)))
                 (rest tok)))
        ((eql (token-kind (first tok)) :ident)
         (let* ((name (token-value (first tok)))
                (var (find-local-var name)))
           (unless var
             (setf var (make-object :name name))
             ;; New object should be in front of the list.
             (appendf *local-variables* (cons var *local-variables*)))
           (values (make-ast-node :kind :variable
                                  :variable var)
                   (rest tok))))
        ((string= (token-value (first tok)) "(")
         (multiple-value-bind (node tokens)
             (parse-expression-node (rest tok))
           (values node (rest (skip-to-token ")" tokens)))))
        (t (error 'parser-error))))

(defun align-to (n align)
  "Round `n' to the nearest multiple of `align'."
  (* (ceiling n align) align))

(defun set-lvar-offsets (prog)
  (let ((offset 0))
    (dolist (var (func-locals prog))
      (incf offset 8)
      (setf (object-offset var) (- offset)))
    (setf (func-stack-size prog) (align-to offset 16))))

(defun parse-program (tok)
  "program ::== statement-node *"
  (let ((nodes '()))
    (loop :until (eql (token-kind (first tok)) :eof)
          :do (multiple-value-bind (node tokens)
                  (parse-statement-node tok)
                (setf nodes (append nodes (list node))
                      tok tokens)))
    (let ((prog (make-func :body nodes :locals *local-variables*)))
      (set-lvar-offsets prog)
      prog)))
