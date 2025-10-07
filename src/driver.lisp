(in-package #:sila)

(defun sila-run (&key (src-file nil) (code nil) (emit-asm t) (stream *standard-output*))
  (unless (and (not (and src-file code))
               (or src-file code))
    (error "Either SRC-FILE or CODE needs to be set."))
  (let* ((lexer (if src-file
                    (lex :src-file src-file)
                    (lex :code code)))
         (parse-tree (parse-program (lexer-tokens lexer))))
    (when emit-asm
      (emit-code parse-tree :stream stream))))

(defparameter *sila-tmp-dir* "/tmp/sila")

(defun sila-compile (&key (src-file nil) (code nil))
  (unless (and (not (and src-file code))
               (or src-file code))
    (error "Either SRC-FILE or CODE needs to be set."))
  (uiop:with-temporary-file (:stream fd :type "s" :directory *sila-tmp-dir* :keep t)
    (let* ((tmp-file (uiop:native-namestring fd))
           (lexer (if src-file
                      (lex :src-file src-file)
                      (lex :code code)))
           (parse-tree (parse-program (lexer-tokens lexer))))
      (emit-code parse-tree :stream fd)
      ;; TODO: Maybe refactor to use as/ld at some point?
      (let ((gcc-command (list "gcc" "-static" tmp-file)))
        (print gcc-command)
        (uiop:run-program gcc-command)))))
