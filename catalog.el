;; ruby
(when (fboundp 'run-ruby)
  (when (< (count-windows) 2) (split-window-vertically))
  (run-ruby "irb")
  (other-window 1)
  (setq ruby-connect-with-repl-filter-functions
	(list (connect-with-repl-filter-function-maker "^irb.*>")))
  (defun ruby-eval (str)
    (let* ((connect-with-repl-filter-functions ruby-connect-with-repl-filter-functions)
	   (result (connect-with-repl str (ruby-proc))))
      (if (string-match "^=> \\(.*\\)\nirb.*>" result) (match-string 1 result) result)))

  (ruby-eval "(1..10).map{|x| x * x}") ; => [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
)

;; scheme
(when (fboundp 'run-scheme)
  (when (< (count-windows) 2) (split-window-vertically))
  (other-window 1)
  (run-scheme "gosh")
  (other-window -1)
  (setq scheme-connect-with-repl-filter-functions
	(list (connect-with-repl-filter-function-maker "^gosh>")))
  (defun scheme-eval (str)
    (let* ((connect-with-repl-filter-functions scheme-connect-with-repl-filter-functions)
	   (result (connect-with-repl str (scheme-proc))))
      (if (string-match "\\(.*\\)\ngosh>" result) (match-string 1 result) result)))
  ;; (defun scheme-eval-ansync (str)
  ;;   (let ((connect-with-repl-filter-functions scheme-connect-with-repl-filter-functions))
  ;;     (connect-with-repl-ansync str (scheme-proc) 'print)))
;; (setq x (run-with-idle-timer  0.1 t (lambda () (print "foo"))))
  ;; (scheme-eval-ansync "(+ 1 2)")
;; (let ((comint-preoutput-filter-functions scheme-connect-with-repl-filter-functions))
;;   (cnw-starting-connect (scheme-proc) "(+ 1 2)"))
;; (connect-with-repl-send (scheme-proc) "(+ 1 2)")
;; (comint-simple-send (scheme-proc) "(+ 1 2)")
  (scheme-eval "(use srfi-1)")
  (scheme-eval "(map (cut * <> 2) (iota 10))") ; => (0 2 4 6 8 10 12 14 16 18)
  )
