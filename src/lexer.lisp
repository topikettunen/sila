(in-package #:sila)

(deftype kind ()
  "Sila token kind"
  '(member
    :error
    :ident
    :punct
    :keyword
    :num
    :eof))

(util:defstruct-read-only token
  (kind :type (or null kind))
  (line 1 :type (or null integer)) ;; One-based offset to the source
  (col 1 :type (or null integer)) ;; One-based offset to the source
  (length 0 :type integer)
  (literal "" :type string))

(defun list-of-tokens-p (list)
  (and (consp list)
       (every #'token-p list)))

(deftype list-of-tokens ()
  `(satisfies list-of-tokens-p))

(defstruct (lexer
            (:copier nil))
  "State for lexical analysis"
  (tokens nil :type (or null list-of-tokens))
  (errors nil :type (or null list-of-tokens))
  (source (util:required-argument 'sourcefile) :type string :read-only t))

(defun whitespacep (c)
  "Predicate for whitespace."
  (member c '(#\Space #\Tab #\Return #\Newline)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Return #\Newline) str))

(defun punctuatorp (c)
  "Predicate for punctuators."
  (member c '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
              #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
              #\[ #\\ #\] #\^ #\_ #\` #\{ #\} #\~)))

(defun skip-to (predicate input start)
  "Skip to position that fullfils the PREDICATE."
  (position-if predicate input :start start))

(defun punct-length (input pos)
  "Read punctuator and return its length. If token isn't a punctuator, return 0."
  (let* ((end (if (= pos (1- (length input)))
                  (length input)
                  (+ pos 2)))
         (punct (subseq input pos end)))
    (cond ((or (string= "==" punct)
               (string= "!=" punct)
               (string= "<=" punct)
               (string= ">=" punct)
               (string= ":=" punct))
           2)
          ((punctuatorp (char input pos))
           1)
          (t 0))))

(defvar *sila-keywords*
  #("return" "if" "else" "for" "loop" "break"))

(defun gen-number-token (src line-no src-pos)
  "Generate token for NUMBER and return it and the SRC-POS to the next token in
SRC."
  (let* ((start-pos src-pos)
         (punct-pos (skip-to #'punctuatorp src src-pos))
         (token-len (if punct-pos
                        (- punct-pos src-pos)
                        ;; No more punctuators.
                        (- (length src) src-pos)))
         (token-val (trim-whitespace
                     (subseq src src-pos (+ src-pos token-len)))))
    ;; Idents starting with a letter will be caught with
    ;; a different conditional so if this is hit, ident
    ;; starts with a number but contains letters, which
    ;; isn't acceptable.
    ;;
    ;; TODO: collect all the errors in some list.
    ;;
    (unless (every #'digit-char-p token-val)
      (error 'lexer-error
             :lexer-input src
             :error-msg "Ident can't start with a number."
             :token-pos src-pos))
    (cond ((not (null punct-pos))
           (setf src-pos punct-pos)
           (when (and (char= (char src src-pos) #\<)
                      (ignore-errors
                       (char= (char src (1+ src-pos)) #\-)))
             (error 'lexer-error
                    :lexer-input src
                    :error-msg "Can't assign to a number."
                    :token-pos src-pos)))
          (t
           (setf src-pos (length src))))
    (values (make-token :kind :num
                        :literal token-val
                        :length (length token-val)
                        :line line-no
                        :col start-pos)
            src-pos)))

(defun gen-ident-or-keyword-token (src line-no src-pos)
  "Generate IDENT or KEYWORD token and return it and the SRC-POS to the next
token in SRC."
  (flet ((keyword-lookup (input pos)
           (let ((keyword-end (skip-to #'whitespacep input pos)))
             ;; Keyword not found
             (when (null keyword-end)
               (return-from keyword-lookup (values nil pos)))
             (let ((keyword (subseq input pos keyword-end)))
               (if (find keyword *sila-keywords* :test #'string=)
                   (values keyword
                           (skip-to #'(lambda (c) (not (whitespacep c)))
                                    input keyword-end))
                   (values nil pos))))))
    (multiple-value-bind (keyword next-token-pos)
        (keyword-lookup src src-pos)
      (let* ((start-pos src-pos)
             (punct-pos (skip-to #'punctuatorp src src-pos))
             (token-len (cond (keyword (length keyword))
                              (punct-pos (- punct-pos src-pos))
                              (t (- (length src) src-pos))))
             (token-val (if keyword
                            keyword
                            (trim-whitespace
                             (subseq src src-pos (+ src-pos token-len))))))
        (setf src-pos (cond (keyword next-token-pos)
                            (punct-pos punct-pos)
                            (t (length src))))
        (values (make-token :kind (if keyword :keyword :ident)
                            :literal token-val
                            :length (length token-val)
                            :line line-no
                            :col start-pos)
                src-pos)))))

(defun gen-punct-token (src line-no src-pos)
  (let* ((start-pos src-pos)
         (punct-len (punct-length src src-pos))
         (val (subseq src src-pos (+ src-pos punct-len))))
    (incf src-pos punct-len)
    (values (make-token :kind :punct
                        :literal val
                        :length punct-len
                        :line line-no
                        :col start-pos)
            src-pos)))

(defun lex-line (line line-no)
  "Generate tokens from the given source code."
  (let* ((tokens (list))
         (src-pos 0))
    (macrolet ((gentoken (kind)
                 (let ((token-gen-fn (intern (format nil "GEN-~a-TOKEN" kind))))
                   `(multiple-value-bind (token pos)
                        (,token-gen-fn line line-no src-pos)
                      (push token tokens)
                      (setf src-pos pos)))))
      (loop :while (< src-pos (length line))
            :do (cond
                  ;; Skip whitespace
                  ((whitespacep (char line src-pos))
                   (incf src-pos))
                  ;; Number
                  ((digit-char-p (char line src-pos))
                   (gentoken number))
                  ;; Ident or keyword
                  ((alpha-char-p (char line src-pos))
                   (gentoken ident-or-keyword))
                  ;; Punctuator
                  ((punctuatorp (char line src-pos))
                   (gentoken punct))
                  (t
                   (error 'lexer-error
                          :lexer-input line
                          :error-msg "Invalid token."
                          :token-pos src-pos)))))
    tokens))

(defun lex (&key (src-file nil) (code nil))
  (unless (or src-file code)
    (error "Either SRC-FILE or CODE needs to be set."))
  (let ((src (if code
                 (list code)       ; list of source lines is expected
                 (uiop:read-file-lines src-file)))
        (tokens '()))
    (dotimes (line-no (length src))
      (push (lex-line (nth line-no src) (1+ line-no)) tokens))
    ;; No more tokens.
    (push (make-token :kind :eof :line nil :col nil) tokens)
    (make-lexer :source (if code code src-file)
                ;; TODO: do I want to flatten this list of list-of-tokens?
                :tokens (nreverse (util:flatten tokens))
                ;; TODO: gather errors
                :errors nil)))

;;;
;;; Lexer error handling
;;;

(defun format-lexer-error (stream pos input msg)
  "Print lexer error in format like:

Lexer error:

1+a1
  ^ msg
"
  (format stream "Lexer error:~%~%~a~%~{~a~}^ ~a"
          input (make-list pos :initial-element #\Space) msg))

(define-condition lexer-error (error)
  ((token-pos :initarg :token-pos
              :initform nil
              :reader token-pos)
   (error-msg :initarg :error-msg
              :initform nil
              :reader error-msg)
   (lexer-input :initarg :lexer-input
                :initform nil
                :reader lexer-input))
  (:report (lambda (condition stream)
             (format-lexer-error stream
                                 (token-pos condition)
                                 (lexer-input condition)
                                 (error-msg condition))))
  (:documentation "Condition for when we encounter invalid token."))
