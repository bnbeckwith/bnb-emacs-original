;; Ugh... I don't really like this, but Emacs 24 is going that way, so
;; I will *try* to like it.


(if (not (file-exists-p "~/.emacs.d/elpa/package.el"))
    (let ((buffer (url-retrieve-synchronously
		   "http://tromey.com/elpa/package-install.el")))
      (save-excursion
	(set-buffer buffer)
	(goto-char (point-min))
	(re-search-forward "^$" nil 'move)
	(eval-region (point) (point-max))
	(kill-buffer (current-buffer)))))

(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
		  ("elpa" . "http://tromey.com/elpa"))))
(package-initialize)

(provide 'package-config)