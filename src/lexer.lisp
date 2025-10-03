(in-package #:sila)

(deftype kind ()
  "Sila token kind"
  '(member
    :ident
    :punct
    :keyword
    :num
    :eof))

(defstruct (token
            (:copier nil))
  (kind (util:required-argument 'kind) :type (or null kind) :read-only t)
  (position 0 :type integer :read-only t)
  (length 0 :type integer :read-only t)
  (value "" :type string :read-only t)
  (next nil :type t))

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

(defun gen-number-token (src src-pos)
  "Generate token for NUMBER and return it and the SRC-POS to the next token in
SRC."
  (let* ((punct-pos (skip-to #'punctuatorp src src-pos))
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
                        :value token-val
                        :length (length token-val)
                        :position src-pos)
            src-pos)))

(defun gen-ident-or-keyword-token (src src-pos)
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
      (let* ((punct-pos (skip-to #'punctuatorp src src-pos))
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
                            :value token-val
                            :length (length token-val)
                            :position src-pos)
                src-pos)))))

(defun gen-punct-token (src src-pos)
  (let* ((punct-len (punct-length src src-pos))
         (val (subseq src src-pos (+ src-pos punct-len))))
    (incf src-pos punct-len)
    (values (make-token :kind :punct
                        :value val
                        :position src-pos
                        :length punct-len)
            src-pos)))

(defun tokenize (src)
  "Generate tokens from the given source code."
  (let* ((head (make-token :kind nil))
         (cur head)
         (src-pos 0))
    (macrolet ((gentoken (kind)
                 (let ((token-gen-fn (intern (format nil "GEN-~a-TOKEN" kind))))
                   `(multiple-value-bind (token pos)
                        (,token-gen-fn src src-pos)
                      (setf (token-next cur) token)
                      (setf cur (token-next cur))
                      (setf src-pos pos)))))
      (loop :while (< src-pos (length src))
            :do (cond
                  ;; Skip whitespace
                  ((whitespacep (char src src-pos))
                   (incf src-pos))
                  ;; Number
                  ((digit-char-p (char src src-pos))
                   (gentoken number))
                  ;; Ident or keyword
                  ((alpha-char-p (char src src-pos))
                   (gentoken ident-or-keyword))
                  ;; Punctuator
                  ((punctuatorp (char src src-pos))
                   (gentoken punct))
                  (t
                   (error 'lexer-error
                          :lexer-input src
                          :error-msg "Invalid token."
                          :token-pos src-pos)))))
    ;; No more tokens.
    (setf (token-next cur) (make-token :kind :eof :position src-pos))
    (setf cur (token-next cur))
    (token-next head)))

(defun print-tokens (tokens)
  "This is mainly used for printing long linked list of tokens, so that they look
slighly better when printing it in REPL. Essentially it just prints the token
like the default function, but it just removes the NEXT slot from it since it
when printing token with NEXT tokens, in REPL, the structure drifts to left a
lot which causes wrapping. Prints to STDERR."
  (loop :for tok := tokens
          :then (setf tok (token-next tok))
        :until (null tok)
        :do (format *error-output*
                    "#S(TOKEN :KIND ~a~c:POSITION ~d~c:LENGTH ~d~c:VALUE ~a)~%"
                    (token-kind tok)
                    #\Tab
                    (token-position tok)
                    #\Tab
                    (token-length tok)
                    #\Tab
                    (token-value tok)))
  (values))

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
