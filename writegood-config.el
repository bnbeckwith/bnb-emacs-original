;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WC-mode

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "writegood-mode/")))

(require 'writegood-mode)

(global-set-key "\C-cg" 'writegood-mode)

(provide 'writegood-config)