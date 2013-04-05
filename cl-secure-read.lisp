;;;; cl-secure-read.lisp

(in-package #:cl-secure-read)

;;; The basis for the following is the code from Let over Lambda book by Doug Hoyte www.letoverlambda.com
;;; The differences include a couple of convenient ways to lessen the stricture imposed on the input.

(defvar safe-read-from-string-blacklist nil
  "Macro characters to disable in a readtable. If NIL, disable all macrocharacters there are.")

(defvar safe-read-from-string-whitelist '(:lists :quotes)
  "Macro characters and features to enable in a readtable.
If NIL, nothing is enabled. Defaults to enabling construction of arbitrary conses.")

(defun analyze-readtable-chars (&optional (readtable *readtable*))
  "Figure out, what characters in readtable are macro, and what are dispatch-macro."
  (let ((dispatch-function (get-macro-character #\# (find-readtable :standard))))
    (iter (for code from 0 below char-code-limit)
	  (let ((char (code-char code)))
	    (if char
		(let ((it (get-macro-character char readtable)))
		  (if it
		      (if (equal it dispatch-function)
			  (collect `(,char
				     ,@(iter (for sub-code from 0 below char-code-limit)
					     (let ((sub-char (code-char sub-code)))
					       (if (and sub-char
							(get-dispatch-macro-character char sub-char
										      readtable))
						   (collect sub-char))))) into dispatch-macro-chars)
			  (collect char into macro-chars))))))
	  (finally (return `((:macro-chars ,@macro-chars) (:dispatch-macro-chars ,@dispatch-macro-chars)))))))
    
(defun expand-white-black-list (lst)
  "Expand keyword abbreviations, found in a whitelist."
  (iter (for elt in lst)
	(cond ((characterp elt) (collect elt into macro-chars))
	      ((and (consp elt) (characterp (car elt))) (collect elt into dispatch-macro-chars))
	      ((keywordp elt) (case elt
				(:lists (appending '(#\( #\)) into macro-chars))
				(:quotes (appending '(#\' #\` #\,) into macro-chars))
				(t (collect elt into perks))))
	      (t (error "Unexpected element of whitelist: ~a" elt)))
	(finally (return `((:macro-chars ,@macro-chars)
			   (:dispatch-macro-chars ,@dispatch-macro-chars)
			   (:perks ,@perks))))))


(defmacro! with-locked-readtable ((name readtable blacklist whitelist) &body body)
  "Parse black and whitelists, lock the readtable, then pass control to the body.
Binds RT to tightened readtable, BLACKLIST and WHITELIST to parsed black- and white-list, respectively.
NAME is the name of a function, which is used in the error report."
  `(let* ((rt (copy-readtable (find-readtable ,readtable)))
	  (blacklist (aif ,blacklist
			  (expand-white-black-list ,e!-it)
			  (analyze-readtable-chars rt)))
	  (whitelist (expand-white-black-list ,whitelist)))
     (let ((,g!-errfun-name (lambda (stream close-char)
			      (declare (ignore stream close-char))
			      (error ,(strcat (string name) " failure")))))
       ;; Disable ordinary macro-chars
       (let ((black-macro-chars (cdr (assoc :macro-chars blacklist)))
	     (white-macro-chars (cdr (assoc :macro-chars whitelist))))
	 (dolist (c black-macro-chars)
	   (if (not (find c white-macro-chars :test #'char=))
	       (set-macro-character c ,g!-errfun-name nil rt))))

       ;; Disable dispatching macro-chars
       (let ((black-dispatching-chars (cdr (assoc :dispatch-macro-chars blacklist)))
	     (white-dispatching-chars (cdr (assoc :dispatch-macro-chars whitelist))))
	 (iter (for (char . sub-chars) in black-dispatching-chars)
	       (let ((wh-sub-chars (cdr (find char white-dispatching-chars :test #'char= :key #'car))))
		 (iter (for sub-char in sub-chars)
		       (if (not (find sub-char wh-sub-chars :test #'char=))
			   (set-dispatch-macro-character char sub-char ,g!-errfun-name rt))))))

       ,@body)))

(defmacro! define-secure-read-from-string (safe-name
			       &key
			       (readtable :standard)
			       (blacklist 'safe-read-from-string-blacklist)
			       (whitelist 'safe-read-from-string-whitelist)
			       fail-value)
  "Define a safer version of READ-FROM-STRING.
READTABLE is a name of a readtable, on base of which to build a 'locked' version of a readtable.
BLACKLIST is a list of macrocharacters and dispatching macro-characters not to allow.
WHITELIST is a list of macrocharacters and dispatching macro-characters to allow."
  `(with-locked-readtable (,safe-name ,readtable ,blacklist ,whitelist)
     (let ((read-eval (find :allow-read-eval (cdr (assoc :perks whitelist))))
	   (io-syntax (find :keep-io-syntax (cdr (assoc :perks whitelist)))))
       ;; (format t "read-eval: ~a~%" read-eval)
       (defun ,safe-name (string &optional (eof-error-p t) eof-value &key (start 0) end preserve-whitespace)
	 (if (stringp string)
	     (macrolet ((frob ()
			  `(let ((*readtable* rt))
			     (let ((*read-eval* (if read-eval *read-eval*)))
			       ;; (format t "*read-eval*: ~a~%" *read-eval*)
			       (handler-bind
			       	   ((error (lambda (condition)
			       		     (declare (ignore condition))
			       		     (return-from
			       		      ,',safe-name ,',fail-value))))
				 (read-from-string string eof-error-p eof-value
						   :start start :end end :preserve-whitespace preserve-whitespace))))))
	       (if io-syntax
		   (frob)
		   (with-standard-io-syntax
		     (frob))))
	     ,fail-value)))))

(defmacro! define-secure-read (safe-name
			       &key
			       (readtable :standard)
			       (blacklist 'safe-read-from-string-blacklist)
			       (whitelist 'safe-read-from-string-whitelist)
			       preserving-whitespace
			       fail-value)
  `(with-locked-readtable (,safe-name ,readtable ,blacklist ,whitelist)
     (let ((read-eval (find :allow-read-eval (cdr (assoc :perks whitelist))))
	   (io-syntax (find :keep-io-syntax (cdr (assoc :perks whitelist)))))
       ;; (format t "read-eval: ~a~%" read-eval)
       (defun ,safe-name (&optional (stream *standard-input*) (eof-error-p t) eof-value recursive-p)
	 (macrolet ((frob ()
		      `(let ((*readtable* rt))
			 (let ((*read-eval* (if read-eval *read-eval*)))
			   ;; (format t "*read-eval*: ~a~%" *read-eval*)
			   (handler-bind
			       ((error (lambda (condition)
			   		 (declare (ignore condition))
			   		 (return-from
			   		  ,',safe-name ,',fail-value))))
			     (,',(if preserving-whitespace
				     'read-preserving-whitespace
				     'read)
				 stream eof-error-p eof-value recursive-p))))))
	   (if io-syntax
	       (frob)
	       (with-standard-io-syntax
		 (frob))))))))
