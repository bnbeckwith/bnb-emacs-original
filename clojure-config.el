;; Clojure mode 

(add-to-list 'load-path (list (expand-file-name
			       (concat bnb-elisp-dir "clojure-mode"))))
(add-to-list 'exec-path "C:/Users/bnbeckwi/bin/")

(autoload 'clojure-mode "clojure-mode" "Paren paradise" t)
(add-to-list 'auto-mode-alist
	     '(".clj" . clojure-mode))
(provide 'clojure-config)


