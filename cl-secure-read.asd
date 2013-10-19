;;;; cl-secure-read.asd

(in-package asdf)

(asdf:defsystem #:cl-secure-read
  :serial t
  :description "Secure lisp reader in spirit of Let over Lambda"
  :author "Alexander Popolitov <popolit@gmail.com>"
  :license "GPLv3"
  :depends-on (#:defmacro-enhance #:rutils #:named-readtables #:iterate #:yaclanapht #:alexandria)
  :components ((:file "package")
               (:file "cl-secure-read")))

(asdf:defsystem #:cl-secure-read-tests
  :depends-on (#:cl-secure-read #:fiveam)
  :licence "GPLv3"
  :components ((:file "tests")))

(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-secure-read))))
  (operate 'load-op :cl-secure-read-tests :force t)
  (operate 'test-op :cl-secure-read-tests :force t))

(defmethod perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-secure-read-tests))))
  (load-system :cl-secure-read-tests)
  (funcall (intern "RUN-TESTS" :cl-secure-read-tests)))
