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
  (defun scheme-eval (str &optional timeout)
    (let* ((connect-with-repl-filter-functions scheme-connect-with-repl-filter-functions)
	   (result (connect-with-repl str (scheme-proc) timeout)))
      (if (string-match "\\(.*\\)\ngosh>" result) (match-string 1 result) result)))
  (scheme-eval "(use srfi-1)")
  (scheme-eval "(for-each print (iota 5))")
  (scheme-eval "(values 1 2 3)")
  (scheme-eval "(map (cut * <> 2) (iota 10))") ; => (0 2 4 6 8 10 12 14 16 18)
  )
