(eval-when-compile (require 'cl))

(defgroup connect-with-repl nil
  "utility for communicate repl (send message and receive their result)"
  :group 'applications
  :prefix "connect-with-repl-")

(defcustom connect-with-repl-wait-time 100
  "wait time(msec) for repl evaluating expression"
  :type 'integer
  :group 'connect-with-repl)

(defcustom connect-with-repl-output-eval-result-p nil
  "calling foo-eval function then output result or not at repl buffer"
  :type 'boolean
  :group 'connect-with-repl)
  
(defvar connect-with-repl-storage-buffer-name "* CWR storage *"
  "storage buffer name")
(defvar connect-with-repl-storage-buffer nil
  "sotered buffer for connected repl's output") 
(defvar %connect-with-repl-mutex% nil
  "internal mutex variable (boolean)")
(defvar connect-with-repl-filter-functions nil
  "special variable this function manages internal mutex while running repl")

;;; utility
(defvar cnw-gensym-function nil)
(defun cnw-gensym-function (&optional recreate?)
  (if (or recreate? (not cnw-gensym-function))
      (setq cnw-gensym-function (format "CNW:%s"  (gensym)))
      (or cnw-gensym-function
	  (cnw-gensym-function t))))

(defmacro with-cnw-storage-buffer-bottom-at  (&rest action)
  `(with-current-buffer connect-with-repl-storage-buffer
    ;(save-excursion
       (goto-char (point-max))
       ,@action))

(defun cnw-repl-finished-p ()
  "if repl's action is finished, 
so gensym-string is existed at end-of-buffer in storage-buffer"
  (with-cnw-storage-buffer-bottom-at
   (looking-back (cnw-gensym-function))))

(defun connect-with-repl-output-result (&optional check-p)
  (when (and check-p (not (cnw-repl-finished-p))) 
    (error "connect-with-repl-output-result: repl is runnnig yet"))
  (with-cnw-storage-buffer-bottom-at
   (forward-line -1)
   (goto-char (point-at-eol))
   (buffer-substring (point-min) (point))))

(defun cnw-storage-buffer-initialize ()
  (unless connect-with-repl-storage-buffer
    (setq connect-with-repl-storage-buffer
	  (get-buffer-create connect-with-repl-storage-buffer-name)))
  (with-current-buffer connect-with-repl-storage-buffer
    (erase-buffer)))

(defun cnw-starting-connect (proc string)
  (cnw-gensym-function t) ;; update gensym-string  
  (cnw-storage-buffer-initialize)
  (connect-with-repl-send proc string))

(defun cnw-wait-loop (timeout)
  (let ((dt connect-with-repl-wait-time))
    (if timeout
    	(while (not (cnw-repl-finished-p))
    	  (unless (> timeout 0) (error "time out"))
    	  (sleep-for 0 dt) 
    	  (decf timeout dt))
    	(while (not (cnw-repl-finished-p))
    	  (sleep-for 0 dt)))))

(defun connect-with-repl (string proc &optional timeout check-p)
  (declare (special comint-preoutput-filter-functions))
  (let ((comint-preoutput-filter-functions connect-with-repl-filter-functions))
    (cnw-starting-connect proc string)
    (cnw-wait-loop timeout)
    (connect-with-repl-output-result check-p)))

(defvar connect-with-repl-ansync-function-timer nil)
(defun connect-with-repl-ansync (string proc cont &optional check-p)
  (declare (special comint-preoutput-filter-functions))
  (let ((comint-preoutput-filter-functions connect-with-repl-filter-functions))
    (cnw-starting-connect proc string)))
    (lexical-let ((cont cont)
		  (check-p check-p))
      (setq connect-with-repl-ansync-function-timer
	    (run-with-idle-timer 
	     (* 0.001 connect-with-repl-wait-time) t
	     (lambda ()
	       (when (cnw-repl-finished-p)
		 (funcall cont (connect-with-repl-output-result check-p))
		 (cancel-timer connect-with-repl-ansync-function-timer))))))))

(defun connect-with-repl-send (proc string)
  (comint-simple-send proc string))

(defmacro connect-with-repl-filter-function-maker (prompt)
  (let ((line (gensym)))
    `(lambda (,line)
       (with-cnw-storage-buffer-bottom-at
	(insert ,line)
	(when (string-match-p ,prompt ,line)
	  (insert (cnw-gensym-function))))
       ,(if connect-with-repl-output-eval-result-p line ""))))
