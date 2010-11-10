;; configuration for slime

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "slime-2010-11-04/")))

;(setq inferior-lisp-program "C:/cygwin/bin/clisp.exe")
(setq inferior-lisp-program "C:\\Programs\\clisp-2.49\\clisp.exe")

(require 'slime-autoloads)
(slime-setup '(slime-fancy))
