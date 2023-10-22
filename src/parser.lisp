(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:alexandria
                #:appendf)
  (:import-from #:sila/lexer
                #:token-kind
                #:token-value
                #:token-next))
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
    :return-statement
    :compound-statement ; { ... }
    :expression-statement
    :variable
    :number))

;; TODO(topi): Set types for these slots.
(defstruct ast-node
  "Structure for Sila AST nodes."
  kind
  body
  value    ; Used if `kind' is :NUMBER.
  variable ; Used if `kind' is :VARIABLE.
  lhs
  rhs
  next)

(defvar *local-variables* nil)

;; TODO(topi): Set types for these slots.
(defstruct object
  "Structure for local variables."
  name
  offset
  next)

;; TODO(topi): Set types for these slots.
(defstruct func
  "Structure for functions."
  body
  locals
  stack-size)

(defun skip-to-token (val tok)
  "Search for token which is VAL. Returns NIL when not found.

TODO(topi): Probably should do some proper error handling if VAL isn't found."
  (cond ((eql (token-kind tok) :eof) nil)
        ((string= (token-value tok) val) tok)
        (t (skip-to-token val (token-next tok)))))

(defun parse-compound-statement-node (tok)
  "compound-statement-node ::== statement-node * '}'"
  (let* ((head (make-ast-node))
         (cur head))
    (loop :until (string= (token-value tok) "}")
          :do (multiple-value-bind (node rest)
                  (parse-statement-node tok)
                (setf (ast-node-next cur) node)
                (setf cur (ast-node-next cur))
                (setf tok rest)))
    (values (make-ast-node :kind :compound-statement
                           :body (ast-node-next head))
            (token-next tok))))

(defun parse-statement-node (tok)
  "statement-node ::== 'return' expression-node ';'
                     | '{' compound-statement-node
                     | expression-statement-node"
  (when (string= (token-value tok) "return")
    (multiple-value-bind (node rest)
        (parse-expression-node (token-next tok))
      (return-from parse-statement-node
        (values (make-ast-node :kind :return-statement
                               :lhs node)
                (token-next (skip-to-token ";" rest))))))
  (when (string= (token-value tok) "{")
    (return-from parse-statement-node
      (parse-compound-statement-node (token-next tok))))
  (parse-expression-statement-node tok))

(defun parse-expression-statement-node (tok)
  "expression-statement-node ::== expression-node ';'"
  (multiple-value-bind (node rest)
      (parse-expression-node tok)
    (values (make-ast-node :kind :expression-statement
                           :lhs node)
            (token-next (skip-to-token ";" rest)))))

(defun parse-expression-node (tok)
  "expression-node ::== assign"
  (parse-assign-node tok))

(defun parse-assign-node (tok)
  "assign-node ::== equality ( '<-' assign ) ?"
  (multiple-value-bind (node rest)
      (parse-equality-node tok)
    (when (string= (token-value rest) "<-")
      (multiple-value-bind (node2 rest2)
          (parse-assign-node (token-next rest))
        (setf node (make-ast-node :kind :assign
                                  :lhs node
                                  :rhs node2))
        (setf rest rest2)))
    (values node rest)))

(defmacro define-binop-parser (name &key descent-parser comparison-symbols bnf)
  "Macro for generating new binary operator parser rules."
  (let ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
        (descent-parser-name (intern (format nil "PARSE-~a-NODE" descent-parser))))
    `(defun ,parser-name (tok)
       ,bnf
       (multiple-value-bind (node rest)
           (,descent-parser-name tok)
         (loop
           (cond
             ,@(loop :for symbol :in comparison-symbols
                     :collect `((string= (token-value rest) ,(car symbol))
                                (multiple-value-bind (node2 rest2)
                                    (,descent-parser-name (token-next rest))
                                  (setf node (make-ast-node :kind ,(cdr symbol)
                                                            :lhs node
                                                            :rhs node2))
                                  (setf rest rest2))))
             (t
              (return-from ,parser-name
                (values node rest)))))))))

(define-binop-parser equality
  :descent-parser relational
  :comparison-symbols (("==" . :equal)
                       ("!=" . :not-equal))
  :bnf "equality-node ::== relational-node ( '==' relational-node | '!=' relational-node ) *")

(define-binop-parser relational
  :descent-parser add
  :comparison-symbols (("<" . :lesser-than)
                       ("<=" . :lesser-or-equal)
                       (">" . :greater-than)
                       (">=" . :greater-or-equal))
  :bnf "relational-node ::== add ( '<'  add | '<=' add | '>'  add | '>=' add ) *")

(define-binop-parser add
  :descent-parser multiplicative
  :comparison-symbols (("+" . :add)
                       ("-" . :sub))
  :bnf "add-node ::== multiplicative-node ( '+' multiplicative-node | '-' multiplicative-node ) *")

(define-binop-parser multiplicative
  :descent-parser unary
  :comparison-symbols (("*" . :mul)
                       ("/" . :div))
  :bnf "multiplicative-node ::== unary-node ( '*' unary-node | '/' unary-node ) *")

(defun parse-unary-node (tok)
  "unary-node ::== ( '+' | '-' ) unary | primary-node"
  (cond ((string= (token-value tok) "+")
         (parse-unary-node (token-next tok)))
        ((string= (token-value tok) "-")
         (multiple-value-bind (node rest)
             (parse-unary-node (token-next tok))
           ;; In case something like '--10' is encountered.
           (when (string= (token-value rest) "-")
             (parse-unary-node (token-next rest)))
           (values (make-ast-node :kind :neg :lhs node)
                   rest)))
        (t
         (parse-primary-node tok))))

(defun find-local-var (name)
  (loop :for obj := *local-variables*
          :then (setf obj (object-next obj))
        :until (null obj)
        :do (when (string= name (object-name obj))
              (return-from find-local-var obj))))

(defun parse-primary-node (tok)
  "primary-node ::== '(' expression-node ')' | ident | number"
  (cond ((eql (token-kind tok) :num)
         (values (make-ast-node :kind :number :value (token-value tok))
                 (token-next tok)))
        ((eql (token-kind tok) :ident)
         (let* ((name (token-value tok))
                (var (find-local-var name)))
           (when (null var)
             (setf var (make-object :name name :next *local-variables*))
             ;; New object should be in front of the list.
             (setf *local-variables* var))
           (values (make-ast-node :kind :variable
                                  :variable var)
                   (token-next tok))))
        ((string= (token-value tok) "(")
         (multiple-value-bind (node rest)
             (parse-expression-node (token-next tok))
           (values node (token-next (skip-to-token ")" rest)))))
        (t (error "Unexpected token value: ~a" tok))))

(defun align-to (n align)
  "Round N to the nearest multiple of ALIGN."
  (* (ceiling n align) align))

(defun set-lvar-offsets (program)
  (let ((offset 0))
     (loop :for obj := (func-locals program)
             :then (setf obj (object-next obj))
           :until (null obj)
           :do (progn
                 (incf offset 8)
                 (setf (object-offset obj) (- offset))))
     (setf (func-stack-size program) (align-to offset 16)))
  (values))

(defun parse-program (tok)
  "program ::== statement-node *"
  (let* ((head (make-ast-node))
         (cur head))
    (loop :until (eql (token-kind tok) :eof)
          :do (multiple-value-bind (node rest)
                  (parse-statement-node tok)
                (setf (ast-node-next cur) node)
                (setf cur (ast-node-next cur))
                (setf tok rest)))
    (let ((program (make-func :body (ast-node-next head)
                              :locals *local-variables*)))
      (set-lvar-offsets program)
      program)))
