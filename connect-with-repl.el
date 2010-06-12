(eval-when-compile (require 'cl))

(defgroup connect-with-repl nil
  "utility for communicate repl (send message and receive their result)"
  :group 'applications
  :prefix "connect-with-repl-")

(defcustom connect-with-repl-wait-time 100
  "wait time(msec) for repl evaluating expression"
  :type 'integer
  :group 'connect-with-repl)

(defvar connect-with-repl-storage nil)	;; store history
(defvar connect-with-repl-tmp-storage nil) ;; using for communication with repl
(defvar %connect-with-repl-mutex% nil)
(defvar %connect-with-repl-running-yetp% nil)
(defvar connect-with-repl-filter-functions nil)

;;; utility
(defmacro cnw-repl-available-when (&rest body)
  `(if %connect-with-repl-mutex%
       (message "associated repl is busy. ignored.")
       (progn ,@body)))

(defmacro cnw-with-mutex (&rest body)
  `(unwind-protect
       (catch 'mutex
	 (setq %connect-with-repl-mutex% t)
	 ,@body)
     (setq %connect-with-repl-mutex% nil)))

(defmacro cnw-with-repl (filter-functions send-action &rest body)
  `(cnw-with-mutex
    (unwind-protect 
	(let ((comint-preoutput-filter-functions ,filter-functions))
	  (condition-case err
	      (progn
		(push connect-with-repl-tmp-storage connect-with-repl-storage)
		(setq %connect-with-repl-running-yetp% t
		      connect-with-repl-tmp-storage nil)
		,send-action
		,@body)
	    (error (pop connect-with-repl-storage) (throw 'mutex err))))
      (setq %connect-with-repl-running-yetp% nil))))

(defun connect-with-repl-send (proc string)
  (comint-simple-send proc string))

(defun connect-with-repl-internal (filters proc message &optional timeout)
  (cnw-repl-available-when
   (cnw-with-repl filters
		  (connect-with-repl-send proc message)
		  (if timeout
		      (while %connect-with-repl-running-yetp%
			(unless (> timeout 0) (error "time out"))
			(sleep-for 0 connect-with-repl-wait-time)
			(decf timeout 100))
		      (while %connect-with-repl-running-yetp%
			(sleep-for 0 connect-with-repl-wait-time)))
		  (setq connect-with-repl-tmp-storage
		  (mapconcat 'identity (nreverse connect-with-repl-tmp-storage) "")))))
  
(defun connect-with-repl (message &optional proc timeout filters) ;timeout is msec
  (let ((proc (if (functionp proc) (funcall proc) proc))
	(filters (or filters connect-with-repl-filter-functions)))
    (connect-with-repl-internal filters proc message timeout)))

(defun connect-with-repl-cleanup-storage () (interactive)
  (setq connect-with-repl-storage nil))

