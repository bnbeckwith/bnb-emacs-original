;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org2blog

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "org2blog/")))
(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "xml-rpc-el/")))

(global-set-key "\C-c2" 'org2blog-mode)

(setq org2blog/wp-blog-alist
      '(("bnbeckwith"
	 :url "http://bnbeckwith.com/xmlrpc.php"
	 :username "Ben"
	 :default-categories ("Blog")
	 :tags-as-categories nil)))

(setq org2blog-keep-new-lines t)

(require 'org2blog)

(add-to-list 'org2blog/wp-sourcecode-langs "elisp")

(provide 'org2blog-config)