;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WC-mode

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "wc-mode/")))

(require 'wc-mode)

(global-set-key "\C-cw" 'wc-mode)

(provide 'wcmode-config)