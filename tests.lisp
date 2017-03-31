;;;; cl-secure-read tests
;;;; See :licence entry in cl-secure-read.asd for details.

(defpackage #:cl-secure-read-tests
  (:use :cl :cl-secure-read :fiveam :iterate)
  (:export #:run-tests))

(in-package cl-secure-read-tests)

(def-suite securead)
(in-suite securead)

(defun run-tests ()
  (let ((results (run 'securead)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(define-secure-read-from-string strict-safe-reader :fail-value "caboom!")

(defmacro mv-equal (form &rest values)
  `(is (equal ',values (multiple-value-list ,form))))

(test strict
  (mv-equal (strict-safe-reader "(+ 1 2 3)") (+ 1 2 3) 9)
  (mv-equal (strict-safe-reader "#.(+ 1 2 3)") "caboom!")
  (mv-equal (strict-safe-reader "; this is the comment") "caboom!")
  (mv-equal (strict-safe-reader "#(1 2 3)") "caboom!"))

(let ((safe-read-from-string-whitelist '(:lists :quotes :allow-read-eval :keep-io-syntax (#\# #\. #\())))
  (define-secure-read-from-string less-strict-safe-reader :fail-value "caboom!"))

(defun vector-to-list (vec)
  (iter (for elt in-vector vec)
        (collect elt)))

(test less-strict
  (mv-equal (less-strict-safe-reader "(+ 1 2 3)") (+ 1 2 3) 9)
  (mv-equal (less-strict-safe-reader "#.(+ 1 2 3)") 6 11)
  (mv-equal (let (*read-eval*)
              (less-strict-safe-reader "#.(+ 1 2 3)"))
            "caboom!")
  (mv-equal (less-strict-safe-reader "; this is the comment") "caboom!")
  (mv-equal (vector-to-list (less-strict-safe-reader "#(1 2 3)")) (1 2 3)))

;; Here we test the lambda not very thoroughly, since it underlies DEFUN'ed versions anyway,
;; and in a sense is tested there.
(defparameter strict-lambda (secure-read-from-string-lambda strict-safe-reader :fail-value "caboom!"))

(test strict-lambda
  (mv-equal (funcall strict-lambda "(+ 1 2 3)") (+ 1 2 3) 9)
  (mv-equal (funcall strict-lambda "#.(+ 1 2 3)") "caboom!")
  (mv-equal (funcall strict-lambda "; this is the comment") "caboom!")
  (mv-equal (funcall strict-lambda "#(1 2 3)") "caboom!"))


;; ;;; Here we test DEFINE-SECURE-READ

(define-secure-read strict-secure-read :fail-value "caboom!")

(test stream-strict
  (mv-equal (strict-secure-read (make-string-input-stream "(+ 1 2 3)")) (+ 1 2 3))
  (mv-equal (strict-secure-read (make-string-input-stream "#.(+ 1 2 3)")) "caboom!")
  (mv-equal (strict-secure-read (make-string-input-stream "; this is the comment")) "caboom!")
  (mv-equal (strict-secure-read (make-string-input-stream "#(1 2 3)")) "caboom!"))

(let ((safe-read-from-string-whitelist '(:lists :quotes :allow-read-eval :keep-io-syntax (#\# #\. #\())))
  (define-secure-read less-strict-secure-read :fail-value "caboom!"))

(test less-stream-strict
  (mv-equal (less-strict-secure-read (make-string-input-stream "(+ 1 2 3)")) (+ 1 2 3))
  (mv-equal (less-strict-secure-read (make-string-input-stream "#.(+ 1 2 3)")) 6)
  (mv-equal (let (*read-eval*)
              (less-strict-secure-read (make-string-input-stream "#.(+ 1 2 3)")))
            "caboom!")
  (mv-equal (less-strict-secure-read (make-string-input-stream "; this is the comment")) "caboom!")
  (mv-equal (vector-to-list (less-strict-secure-read (make-string-input-stream "#(1 2 3)"))) (1 2 3)))
