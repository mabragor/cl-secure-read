;;;; package.lisp

(defpackage #:cl-secure-read
  (:use #:cl #:defmacro-enhance #:rutils.string #:named-readtables #:iterate #:yaclanapht)
  (:shadowing-import-from #:alexandria #:named-lambda)
  (:export #:define-secure-read-from-string #:define-secure-read
           #:secure-read-from-string-lambda #:secure-read-lambda
	   ::safe-read-from-string-blacklist ::safe-read-from-string-whitelist))

