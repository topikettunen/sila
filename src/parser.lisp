(defpackage #:sila/parser
  (:use #:cl)
  (:import-from #:sila/conditions
                #:parser-error)
  (:import-from #:sila/lexer
                #:tokenize
                #:token-kind
                #:token-val
                #:token-next)
  (:export #:parse-expression-node))
(in-package #:sila/parser)

(deftype ast-node-kind ()
  "Sila AST node kind."
  '(member
    :add     ; +
    :sub     ; -
    :mul     ; *
    :div     ; /
    :neg     ; unary -
    :equal
    :not-equal
    :lesser-than
    :lesser-or-equal
    :greater-than
    :greater-or-equal
    :number))

(defstruct ast-node
  "Structure for Sila AST nodes."
  kind
  val
  lhs
  rhs)

(defun skip-to-token (val tok)
  "Search for token which is `val'. Returns `nil' when not found.

TODO(topi): Probably should do some proper error handling if `val' isn't
found."
  (cond ((eq (token-kind tok) :eof) nil)
        ((equal (token-val tok) val) tok)
        (t (skip-to-token val (token-next tok)))))

(defmacro define-parser (name &key descent-parser
                                   comparison-symbols
                                   bnf)
  "Macro for generating new parser rules."
  (let* ((parser-name (intern (format nil "PARSE-~a-NODE" name)))
         (descent-parser (intern (format nil "PARSE-~a-NODE"  descent-parser))))
    `(defun ,parser-name (tok)
       ,bnf
       (multiple-value-bind (node rest)
           (,descent-parser tok)
         (loop
           (cond
             ,@(loop :for symbol in comparison-symbols
                     :collect `((string= (token-val rest) ,(car symbol))
                                (multiple-value-bind (node2 rest2)
                                    (,descent-parser (token-next rest))
                                  (setf node (make-ast-node :kind ,(cdr symbol)
                                                            :lhs node
                                                            :rhs node2))
                                  (setf rest rest2))))
             (t
              (return-from ,parser-name
                (values node rest)))))))))

;;; Top-most parser function is just defined by calling equality node, so for
;;; now, it doesn't require using macro for defining the rule.
(defun parse-expression-node (tok)
  "expression-node ::== equality"
  (parse-equality-node tok))

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
  (cond ((string= (token-val tok) "+")
         (parse-unary-node (token-next tok)))
        ((string= (token-val tok) "-")
         (multiple-value-bind (node rest)
             (parse-unary-node (token-next tok))
           ;; In case something like '--10' is encountered.
           (when (eq (token-val rest) #\-)
             (parse-unary-node (token-next rest)))
           (values (make-ast-node :kind :neg :lhs node)
                   rest)))
        (t
         (parse-primary-node tok))))

(defun parse-primary-node (tok)
  "primary-node ::== '(' expression-node ')' | number"
  (cond ((eq (token-kind tok) :num)
         (values (make-ast-node :kind :number :val (token-val tok))
                 (token-next tok)))
        ((string= (token-val tok) "(")
         (multiple-value-bind (node rest)
             (parse-expression-node (token-next tok))
           (values node (token-next (skip-to-token ")" rest)))))
        (t (error 'parser-error))))
