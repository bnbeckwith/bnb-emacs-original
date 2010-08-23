;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org2blog

(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "org2blog/")))
(add-to-list 'load-path (expand-file-name
			 (concat bnb-elisp-dir "xml-rpc-el/")))


(global-set-key "\C-c2" 'org2blog-mode)

(setq org2blog-server-url "http://bnbeckwith.com/xmlrpc.php"
      org2blog-server-user "Ben")

(defun bnb/org2blog-src-blocks-to-wp-syntaxhighlighter ()
  "Export #+BEGIN_SRC blocks as Wordpress Syntaxhighlighter
tags. There is a special header option, :syntaxhl that contains
the options to pass to syntaxhighlighter.

This is intended to be added to `org-export-preprocess-hooks'"
  (interactive)
  (save-window-excursion
    (let ((case-fold-search t)
	  (colon-re "^[ \t]*:\\([ \t]\\|$\\)")
	  lang body headers syntaxhl
	  beg)
      (goto-char (point-min))
      (while (re-search-forward colon-re nil t)
	(replace-match (match-string 1))
	(beginning-of-line 1)
	(insert "[text light=\"true\"]\n")
	(setq beg (point))
	(while (looking-at colon-re)
	  (replace-match (match-string 1))
	  (end-of-line 1)
	  (or (eobp) (forward-char 1)))
	(end-of-line 1)
	(add-text-properties beg
			     (if (bolp) 
				 (1- (point)) 
			       (point))
			     '(org-protected t))
	(insert "\n[/text]"))
      (unless (boundp 'org-babel-src-block-regexp)
	(require 'ob))
      (while (re-search-forward
	      (concat "\\(" org-babel-src-block-regexp
		      "\\|" org-babel-inline-src-block-regexp
		      "\\)")
	      nil t)
	(setq lang (match-string-no-properties 3))
	(if (string-match "-" lang)
	    (error "SyntaxHighlighter does not support languages with '-' in the names"))
	(setq headers (match-string-no-properties 5))
	(setq body (match-string-no-properties 6))
	(save-match-data 
	  (setq syntaxhl
		(if (string-match ":syntaxhl[ ]+\\([^ ]+\\)" headers)
		    (concat " " (replace-regexp-in-string "\;" " " (match-string 1 headers))))))
	(replace-match      
	 (concat "\n\n[" lang syntaxhl "]\n" body "[/" lang "]\n")
	 nil t)))))

; I couldn't get this to work any other way.
(setq org-export-preprocess-hook
      (list
       'bnb/org2blog-src-blocks-to-wp-syntaxhighlighter
       'org-export-blocks-preprocess))

(defadvice org2blog-strip-new-lines (around bnb/org2blog-disable-newline-removal)
  "Disables the function the removes newlines from the generated HTML")
  

(require 'org2blog)

(provide 'org2blog-config)