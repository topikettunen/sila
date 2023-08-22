(defpackage #:sila/lexer
  (:use #:cl)
  (:import-from #:sila/conditions
                #:lexer-error)
  (:export #:tokenize
           #:token-kind
           #:token-val
           #:token-next))
(in-package #:sila/lexer)

(deftype kind ()
  "Sila token kind"
  '(member
    :punct
    :num
    :eof))

(defstruct token
  "Structure for Sila tokens."
  kind
  pos
  len
  val
  next)

(defun whitespacep (c)
  "Predicate for whitespace."
  (member c '(#\Space #\Tab #\Return #\Newline)))

(defun punctuatorp (c)
  "Predicate for punctuators. All the punctuators are:

! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
  (member c '(#\! #\" #\# #\$ #\% #\& #\' #\( #\) #\* #\+
              #\, #\- #\. #\/ #\: #\; #\< #\= #\> #\? #\@
              #\[ #\\ #\] #\^ #\_ #\` #\{ #\} #\~)))

(defun skip-to-punctuator (input start)
  "Skip to nearest punctuator."
  ;; TODO(topi): Improve this. Won't work when new tokens are added.
  (position-if-not #'digit-char-p input :start start))

(defun tokenize (src)
  "Generate tokens from the given source code."
  (let* ((head (make-token))
         (cur head)
         (src-pos 0))
    (loop :while (< src-pos (length src))
          :do (let ((punct-pos (skip-to-punctuator src src-pos)))
                (cond (;; Whitespace
                       (whitespacep (char src src-pos))
                       (incf src-pos))
                      (;; Numeric literal
                       (digit-char-p (char src src-pos))
                       (setf (token-next cur)
                             (make-token :kind :num
                                         :val (parse-integer src
                                                             :start src-pos
                                                             :junk-allowed t)
                                         :pos src-pos))
                       (setf (token-len (token-next cur))
                             (if punct-pos
                                 (- punct-pos src-pos)
                                 ;; No more punctuators.
                                 (- (length src) src-pos)))
                       (setf cur (token-next cur))
                       (if punct-pos
                           (setf src-pos punct-pos)
                           ;; No more punctuators, move `src-pos` to the end.
                           (setf src-pos (length src))))
                      (;; Punctuator: + | -
                       (punctuatorp (char src src-pos))
                       (setf (token-next cur)
                             (make-token :kind :punct
                                         :val (char src src-pos)
                                         :pos src-pos
                                         ;; TODO(topi): Currently I only have
                                         ;; 1 char puncts, change when more is
                                         ;; needed.
                                         :len 1))
                       (setf cur (token-next cur))
                       (incf src-pos))
                      (t
                       (error 'lexer-error
                              :lexer-input src
                              :error-msg "Invalid token."
                              :token-position src-pos)))))
    ;; No more tokens.
    (setf (token-next cur) (make-token :kind :eof
                                       :pos src-pos))
    (setf cur (token-next cur))
    (token-next head)))
