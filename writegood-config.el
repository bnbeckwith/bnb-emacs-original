;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WC-mode

(autoload 'writegood-mode "writegood-mode" "Bad writing checker" t)
;;(require 'writegood-mode)

(global-set-key "\C-cg" 'writegood-mode)

(provide 'writegood-config)
