;;;; cl-secure-read tests
;;;; See :licence entry in cl-secure-read.asd for details.

(defpackage #:cl-secure-read-test
  (:use :cl :cl-secure-read :rt))

(in-package cl-secure-read-test)

(define-safe-reader strict-safe-reader)

(deftest strict.1
    (strict-safe-reader "(+ 1 2 3)" "caboom!")
  (+ 1 2 3) 9)

(deftest strict.2
    (strict-safe-reader "#.(+ 1 2 3)" "caboom!")
  "caboom!")

(deftest strict.3
    (strict-safe-reader "; this is the comment" "caboom!")
  "caboom!")

(deftest strict.4
    (strict-safe-reader "#(1 2 3)" "caboom!")
  "caboom!")

(let ((safe-read-from-string-whitelist '(:lists :quotes :allow-read-eval :keep-io-syntax (#\# #\. #\())))
  (define-safe-reader less-strict-safe-reader))

(deftest less-strict.1
    (less-strict-safe-reader "(+ 1 2 3)" "caboom!")
  (+ 1 2 3) 9)

(deftest less-strict.2
    (less-strict-safe-reader "#.(+ 1 2 3)" "caboom!")
  6 11)

(deftest less-strict.2.5
    (let (*read-eval*)
      (less-strict-safe-reader "#.(+ 1 2 3)" "caboom!"))
  "caboom!")

(deftest less-strict.3
    (less-strict-safe-reader "; this is the comment" "caboom!")
  "caboom!")

(deftest less-strict.4
    (less-strict-safe-reader "#(1 2 3)" "caboom!")
  #(1 2 3) 8)

  
			
