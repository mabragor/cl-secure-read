;;;; cl-secure-read.asd

(defsystem "cl-secure-read"
  :serial t
  :description "Secure lisp reader in spirit of Let over Lambda"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPLv3"
  :depends-on ("defmacro-enhance" "rutils" "named-readtables" "iterate" "yaclanapht" "alexandria")
  :components ((:file "package")
               (:file "cl-secure-read"))
  :in-order-to ((test-op (test-op "cl-secure-read/tests"))))

(defsystem "cl-secure-read/tests"
  :depends-on ("cl-secure-read" "fiveam")
  :licence "GPLv3"
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :cl-secure-read-tests :run-tests)))
