;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YASnippet

(setq bnb/yaspath (expand-file-name
			 (concat bnb-elisp-dir 
				 "yasnippet-0.6.1c/")))
(add-to-list 'load-path bnb/yaspath)

(require 'yasnippet)

(yas/initialize)

(yas/load-directory (concat bnb/yaspath "snippets"))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
	  (lambda ()
	    ;; yasnippet (using the new org-cycle hooks)
	    (make-variable-buffer-local 'yas/trigger-key)
	    (setq yas/trigger-key [tab])
	    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	    (define-key yas/keymap [tab] 'yas/next-field)
	    (yas/minor-mode t)))

(provide 'yas-config)