;; configuration for slime

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "slime")))

(require 'slime-autoloads)
(slime-setup '(slime-fancy))
