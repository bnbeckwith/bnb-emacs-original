(add-to-list 'load-path (list (expand-file-name
			       (concat bnb-elisp-dir "swank-clojure"))
			      (expand-file-name
			       (concat bnb-elisp-dir "clojure-mode"))))

(require 'clojure-mode)
(provide 'clojure-config)

