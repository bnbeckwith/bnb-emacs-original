
;Command to open a file using 'emacs'
(defun eshell/emacs (&rest args)
  "Open a file in emacs the natural way"
  (if (null args)
      ;; If emacs is called by itself, then just go to emacs directly
      (bury-buffer)
    ;; If opening multiple files with a directory name, e.g.
    ;; > emacs bar/bar.txt foo.txt
    ;; then the names must be expanded to complete file paths.
    ;; Otherwise, find-file will look in the current directory which
    ;; would fail for 'foo.txt' in the example above.
    (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(provide 'eshell-config)