;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ledger

(setq bnb/ledgerpath (expand-file-name
			 (concat bnb-elisp-dir 
				 "ledger/lisp/")))
(add-to-list 'load-path bnb/ledgerpath)

(require 'ledger)

(provide 'ledger-config)