;; Clojure mode 

(when (eq 'w32 window-system)
  (add-to-list 'exec-path "C:/Users/bnbeckwi/bin/"))

(autoload 'clojure-mode "clojure-mode" "Paren paradise" t)
(add-to-list 'auto-mode-alist
	     '(".clj" . clojure-mode))
(provide 'clojure-config)


