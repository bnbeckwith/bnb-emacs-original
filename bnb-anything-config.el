(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "M-SPC") 'anything)
(setq anything-sources
      (list anything-c-source-buffers
	    anything-c-source-recentf
	    anything-c-source-bookmarks
            anything-c-source-kill-ring
	    anything-c-source-files-in-current-dir+
	    anything-c-source-org-headline
            ))


(global-set-key (kbd "C-c i")
		(lambda () (interactive)
		  (anything
		   :prompt "Switch to: "
		   :candidate-number-limit 3
		   :sources
		   '( anything-c-source-man-pages
		      anything-c-source-info-emacs
		      anything-c-source-info-org
		      anything-c-source-info-latex))))

;; (global-set-key (kbd "C-c i")
;;		(lambda () (interactive)
;;		  (anything
;;:prompt "Crapola"
;;:candidate-number-limit 5
;;:sources
;;		   '( anything-info-at-point))))
			    

;; (add-hook 'emacs-lisp-mode-hook
;;   (lambda()
;;   ;; other stuff...
;;   ;; ...
;;   ;; put useful info under C-c i
;;     (local-set-key (kbd "C-c i")
;;       (lambda() (interactive)
;;         (anything
;;           :prompt "Info about: "
;;           :candidate-number-limit 5
;;           :sources
;;           '( anything-c-source-emacs-functions
;;              anything-c-source-emacs-variables
;;              anything-c-source-info-elisp
;;              anything-c-source-emacs-commands
;;              anything-c-source-emacs-source-defun
;;              anything-c-source-emacs-lisp-expectations
;;              anything-c-source-emacs-lisp-toplevels
;;              anything-c-source-emacs-functions-with-abbrevs
;;              anything-c-source-info-emacs))))

(provide 'bnb-anything-config)