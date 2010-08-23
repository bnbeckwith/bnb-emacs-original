;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org2blog

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "org2blog/")))
(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "xml-rpc-el/")))


(global-set-key "\C-c2" 'org2blog-mode)

(setq org2blog-server-url "http://bnbeckwith.com/xmlrpc.php"
      org2blog-server-user "Ben")

(require 'org2blog)

(provide 'org2blog-config)