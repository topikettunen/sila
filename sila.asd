(defsystem "sila"
  :version "0.0.1"
  :author "Topi Kettunen <topi@topikettunen.com>"
  :license "MIT"
  :depends-on ("unix-opts")
  :components ((:module "src"
                :components
                ((:file "conditions")
                 (:file "lexer")
                 (:file "sila"))))
  :description "Sila programming language"
  :build-operation "program-op" ;; leave as is
  :build-pathname "sila"
  :entry-point "sila:main"
  :in-order-to ((test-op (test-op "sila/tests"))))

(defsystem "sila/tests"
  :author "Topi Kettunen <topi@topikettunen.com>"
  :license "MIT"
  :depends-on ("sila"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "sila")
                 (:file "emit-asm-x86-64"))))
  :description "Test system for sila"
  :perform (test-op (op c) (symbol-call :rove :run c)))
