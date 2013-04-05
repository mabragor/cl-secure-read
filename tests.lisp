;;;; cl-secure-read tests
;;;; See :licence entry in cl-secure-read.asd for details.

(defpackage #:cl-secure-read-test
  (:use :cl :cl-secure-read :rt))

(in-package cl-secure-read-test)

(define-secure-read-from-string strict-safe-reader :fail-value "caboom!")

(deftest strict.1
    (strict-safe-reader "(+ 1 2 3)")
  (+ 1 2 3) 9)

(deftest strict.2
    (strict-safe-reader "#.(+ 1 2 3)")
  "caboom!")

(deftest strict.3
    (strict-safe-reader "; this is the comment")
  "caboom!")

(deftest strict.4
    (strict-safe-reader "#(1 2 3)")
  "caboom!")

(let ((safe-read-from-string-whitelist '(:lists :quotes :allow-read-eval :keep-io-syntax (#\# #\. #\())))
  (define-secure-read-from-string less-strict-safe-reader :fail-value "caboom!"))

(deftest less-strict.1
    (less-strict-safe-reader "(+ 1 2 3)")
  (+ 1 2 3) 9)

(deftest less-strict.2
    (less-strict-safe-reader "#.(+ 1 2 3)")
  6 11)

(deftest less-strict.2.5
    (let (*read-eval*)
      (less-strict-safe-reader "#.(+ 1 2 3)"))
  "caboom!")

(deftest less-strict.3
    (less-strict-safe-reader "; this is the comment")
  "caboom!")

(deftest less-strict.4
    (less-strict-safe-reader "#(1 2 3)")
  #(1 2 3) 8)

;;; Here we test DEFINE-SECURE-READ  
			
(define-secure-read strict-secure-read :fail-value "caboom!")

(deftest stream-strict.1
    (strict-secure-read (make-string-input-stream "(+ 1 2 3)"))
  (+ 1 2 3))

(deftest stream-strict.2
    (strict-secure-read (make-string-input-stream "#.(+ 1 2 3)"))
  "caboom!")

(deftest stream-strict.3
    (strict-secure-read (make-string-input-stream "; this is the comment"))
  "caboom!")

(deftest stream-strict.4
    (strict-secure-read (make-string-input-stream "#(1 2 3)"))
  "caboom!")

(let ((safe-read-from-string-whitelist '(:lists :quotes :allow-read-eval :keep-io-syntax (#\# #\. #\())))
  (define-secure-read less-strict-secure-read :fail-value "caboom!"))

(deftest less-stream-strict.1
    (less-strict-secure-read (make-string-input-stream "(+ 1 2 3)"))
  (+ 1 2 3))

(deftest less-stream-strict.2
    (less-strict-secure-read (make-string-input-stream "#.(+ 1 2 3)"))
  6)

(deftest less-stream-strict.2.5
    (let (*read-eval*)
      (less-strict-secure-read (make-string-input-stream "#.(+ 1 2 3)")))
  "caboom!")

(deftest less-stream-strict.3
    (less-strict-secure-read (make-string-input-stream "; this is the comment"))
  "caboom!")

(deftest less-stream-strict.4
    (less-strict-secure-read (make-string-input-stream "#(1 2 3)"))
  #(1 2 3))

  
			
