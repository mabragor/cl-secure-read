;;;; package.lisp

(defpackage #:cl-secure-read
  (:use #:cl #:rutils.string #:named-readtables #:iterate)
  (:shadowing-import-from #:alexandria #:named-lambda #:with-gensyms)
  (:export #:define-secure-read-from-string #:define-secure-read
           #:secure-read-from-string-lambda #:secure-read-lambda
	   ::safe-read-from-string-blacklist ::safe-read-from-string-whitelist))

