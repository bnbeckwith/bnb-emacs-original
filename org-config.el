;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode

(setq bnb-org-location "~/elisp/org-mode/")

;; Tell emacs where to find it and when to start it
(add-to-list 'load-path (expand-file-name (concat bnb-org-location "lisp/"))
(add-to-list 'load-path (expand-file-name (concat bnb-org-location "contrib/lisp")))
;; .org .org_archive and .org.gpg files are org-mode files
(add-to-list 'auto-mode-alist '("\\.org\\(.gpg|_archive\\)?$" . org-mode))

; Install org-mode
(require 'org-install)
(require 'org-protocol)

; Set up some keystrokes
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-remember)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9> I") 
		'(lambda () (interactive) (info (concat bnb-org-location "doc/org"))))
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> i") 'bnb/clock-in)
(global-set-key (kbd "<f9> o") 'bnb/clock-out)
(global-set-key (kbd "<f11> i") 'org-clock-in)
(global-set-key (kbd "<f11> g") 'org-clock-goto)
(global-set-key (kbd "<f5> s") 'flyspell-buffer)

;; EXPERIMENTAL


(setq org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
				 (sequence "WAITING(w@/!)" "SOMEDAY(s!)" "|" "CANCELED(c@/!)")
				 (sequence "OPEN(O)" "|" "CLOSED(C)"))))

(setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
 ("NEXT" :foreground "blue" :weight bold)
 ("DONE" :foreground "forest green" :weight bold)
 ("WAITING" :foreground "yellow" :weight bold)
 ("SOMEDAY" :foreground "goldenrod" :weight bold)
 ("CANCELED" :foreground "orangered" :weight bold)
 ("OPEN" :foreground "magenta" :weight bold)
 ("CLOSED" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELED"
               ("CANCELED" . t))
              ("WAITING"
               ("WAITING" . t))
              ("SOMEDAY"
               ("WAITING" . t))
              (done
               ("WAITING"))
              ("TODO"
               ("WAITING")
               ("CANCELED"))
              ("NEXT"
               ("WAITING"))
              ("DONE"
               ("WAITING")
               ("CANCELED")))))

;;;  Load Org Remember Stuff
(require 'remember)
(org-remember-insinuate)

;; Start clock in a remember buffer and switch back to previous clocking task on save
(add-hook 'remember-mode-hook 'org-clock-in 'append)
(add-hook 'org-remember-before-finalize-hook 'bnb/clock-in-interrupted-task)

;; I use C-M-r to start org-remember
(global-set-key (kbd "C-M-r") 'org-remember)

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates (quote (("todo" ?t "* TODO %?\n  %U\n  %a" nil bottom nil)
                                     ("note" ?n "* %?                                                                            :NOTE:\n  %U\n  %a\n  :CLOCK:\n  :END:" nil bottom nil)
;                                     ("appointment" ?a "* %?\n  %U" "~/git/org/todo.org" "Appointments" nil)
                                     ("org-protocol" ?w "* TODO Review %c%!\n  %U" nil bottom nil))))

(defun bnb/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "CLOCK" (point))))

(add-hook 'org-clock-out-hook 'bnb/remove-empty-drawer-on-clock-out 'append)


(setq org-agenda-show-inherited-tags t)
;(setq org-agenda-skip-scheduled-if-done t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-export-html-inline-images t)
(setq org-export-with-LaTeX-fragments t)
(setq org-file-apps (quote ((auto-mode . emacs) ("\\.x?html?\\'" . default) ("\\.pdf\\'" . default) ("\\.mm\\'" . default))))
(setq org-goto-interface (quote outline-path-completion))
(setq org-log-done (quote note))
(setq org-log-into-drawer t)
(setq org-modules (quote (org-bbdb org-bibtex org-gnus org-info org-jsinfo org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m org-bookmark org-browser-url org-depend org-R org-toc)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
(setq org-refile-use-outline-path t)
(setq org-remember-default-headline "Tasks")
(setq org-special-ctrl-k t)
(setq org-stuck-projects (quote ("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") ("REFERENCE" "ARCHIVE") "")))
(setq org-use-speed-commands t)
(setq org-agenda-time-grid (quote ((daily today) "----------------" (800 1000 1200 1400 1600 1800 2000))))
(setq org-attach-method (quote ln))
(setq org-clock-into-drawer "LOGBOOK")
(setq org-completion-use-ido t)


(setq org-agenda-custom-commands
      (quote (("w" "Tasks waiting on something" tags "WAITING/!"
               ((org-use-tag-inheritance nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-with-date nil)
                (org-agenda-overriding-header "Waiting Tasks")))
              ("r" "Refile New Notes and Tasks" tags "LEVEL=1+REFILE"
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-overriding-header "Tasks to Refile")))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")))
              ("n" "Next" tags-todo "-WAITING-CANCELED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")))
              ("p" "Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELED"
               ((org-agenda-skip-function 'bh/skip-non-projects)
                (org-agenda-overriding-header "Projects")))
              ("o" "Other (Non-Project) tasks" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELED"
               ((org-agenda-skip-function 'bh/skip-projects)
                (org-agenda-overriding-header "Other Non-Project Tasks")))
              ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELED"
               ((org-agenda-overriding-header "Tasks to Archive")))
              ("h" "Habits" tags "STYLE=\"habit\""
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-overriding-header "Habits")))
              ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE|LEVEL=1+REFILE/!-DONE-CANCELED"
               ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                (org-agenda-overriding-header "Stuck Projects")))
              ("c" "Select default clocking task" tags "LEVEL=2-REFILE"
               ((org-agenda-skip-function
                 '(org-agenda-skip-subtree-if 'notregexp "^\\*\\* Organization"))
                (org-agenda-overriding-header "Set default clocking task with C-u C-u I"))))))


;; Resume clocking tasks when emacs is restarted.
(org-clock-persistence-insinuate)
;; Maybe too long?
(setq org-clock-history-length 28)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
(setq org-clock-in-switch-to-state (quote bnb/clock-in-to-next))
;; Separate drawers for clocking and logs
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))
(setq org-clock-into-drawer "CLOCK")
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-out-when-done t)
(setq org-clock-persist 'history)
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
(setq org-clock-report-include-clocking-task t)

(setq bnb/keep-clock-running nil)

(defun bnb/clock-in ()
  (interactive)
  (setq bnb/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
	(bnb/clock-in-default-task))
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))

(defun bnb/clock-out ()
  (interactive)
  (setq bnb/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun bnb/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bnb/clock-out-maybe ()
  (when (and bnb/keep-clock-running (not org-clock-clocking-in) (marker-buffer org-clock-default-task))
    (bnb/clock-in-default-task)))

(add-hook 'org-clock-out-hook 'bnb/clock-out-maybe 'append)

(require 'org-id)
(defun bnb/clock-in-task-by-id (id)
  "Clock in a task by ID"
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find id 'marker)
      (org-clock-in nil))))

(defun bnb/clock-in-interrupted-task ()
  "Clock in the interrupted task if there is one"
  (interactive)
  (let ((clock-in-to-task))
    (if (org-clock-is-active)
	(when (marker-buffer org-clock-interrupted-task)
	  (if (equal org-clock-interrupted-task org-clock-hd-marker)
	      (setq clock-in-to-task (cadr org-clock-histor))
	    (setq clock-in-to-task org-clock-interrupted-task))))
    (if clock-in-to-task
	(org-with-point-at clock-in-to-task
	  (org-clock-in nil))
      (org-clock-out))))

(setq org-agenda-log-mode-items '(clock))


(defun bnb/clock-in-to-next (kw)
  "Switch task from TODO to NEXT when clocking in.
Skips remember tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
	   (not (string-equal (buffer-name) "*Remember*")))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
	    (has-subtask nil))
	(save-excursion
	  (forward-line 1)
	  (while (and (not has-subtask)
		      (< (point) subtree-end)
		      (re-search-forward "^\*+ " subtree-end t))
	    (when (member (org-get-todo-state) org-not-done-keywords)
	      (setq has-subtask t))))
	(when (not has-subtask)
	  "NEXT"))))

(setq org-clock-in-switch-to-state (quote bnb/clock-in-to-next))

(setq org-time-stamp-rounding-minutes (quote (1 15)))

; Agenda clock report parameters (no links, 2 levels deep)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

(setq org-hide-leading-stars t)

(setq org-odd-levels-only nil)

(setq org-cycle-separator-lines 0)

(setq org-reverse-note-order nil)

(setq org-insert-heading-respect-content t)

(setq org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-protocol org-w3m)))
(setq org-global-properties (quote (("STYLE_ALL" . "habit"))))
(setq org-habit-graph-column 50)


(setq global-auto-revert-mode t)


(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
				      ("1" . delete-other-windows)
				      ("2" . split-window-vertically)
				      ("3" . split-window-horizontally)
				      ("h" . hide-other)
				      ("k" . org-kill-node-or-show-branches)
				      ("r" . org-reveal)
				      ("s" . org-save-all-org-buffers)
				      ("z" . org-add-note))))


(add-to-list 'Info-default-directory-list "~/.emacs.d/org/org-mode/doc/org")

(run-at-time "00:59" 3600 'org-save-all-org-buffers)
;; end-EXPERIMENTAL



; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; Keybindings
	    (local-set-key "\M-I" 'org-toggle-iimage-in-org)
	    ;; yasnippet
	    (make-variable-buffer-local 'yas/trigger-key)
	    (setq yas/trigger-key [tab])
	    (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
	    (yas/minor-mode)
	    ;; Turn on flyspell mode
					;(flyspell-mode 1)
	    ))

; Set up the summary to use.
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
; turn off logging
  (let (org-log-done org-log-states) 
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; Org-protocol
(defun bnb/screenshot-protocol-handler-function (fname)
  "Process an org-protocol://store-screenshot:// style url
and store the file path as an org link.  Also pushes the URL to the `kill-ring'."
  (let* ((uri (replace-regexp-in-string " " "%20" (org-protocol-sanitize-uri fname)))
         (title fname)
         orglink)
    (if (boundp 'org-stored-links)
        (setq org-stored-links (cons (list (concat "file:" uri) title) org-stored-links)))
    (kill-new uri)
    (message "`%s' to insert new org-link, `%s' to insert `%s'"
             (substitute-command-keys"\\[org-insert-link]")
             (substitute-command-keys"\\[yank]")
             uri))
  nil)

(setq org-protocol-protocol-alist
      '(("store-screenshot"
         :protocol "store-screenshot"
         :function bnb/screenshot-protocol-handler-function
         :kill-client t))
      )

(require 'iimage)
(add-to-list 'iimage-mode-image-regex-alist
	     (cons (concat "\\[\\[file:[a-zA-Z]:?\\(~?" 
			   (concat "[-+./_0-9a-zA-Z%]+\\."
				   (regexp-opt (nconc (mapcar #'upcase
							      image-file-name-extensions)
						      image-file-name-extensions)
					       t))
 			   "\\)\\]")  1))

(defadvice iimage-locate-file (before bnb/reinsert-spaces activate)
  "Change %20 in the filename back into proper spaces"
  (ad-set-arg 0 (replace-regexp-in-string "%20" " " (ad-get-arg 0))))

(defun org-toggle-iimage-in-org ()
  (interactive)
  (let ((turning-on (not iimage-mode)))
    (set-face-underline-p 'org-link (not turning-on))
    (iimage-mode (or turning-on 0))))


;; Add some hooks
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; (add-hook 'org-mode-hook 'flyspell-mode)
(defadvice ispell-command-loop (before ispell-reverse-miss-list activate)
  "reverse the first argument to ispell-command-loop"
  (ad-set-arg 0 (reverse (ad-get-arg 0))))

;; Enable remember with org-mode
;;(org-remember-insinuate)

;; Org Feeds
(require 'org-feed)
(setq org-feed-alist
      '(("ReQall"
         "http://www.reqall.com/user/feeds/rss/ef1f7fd093ca0a7d98ba5758d64b00775c47ccf3"
         "c:/Documents and Settings/bnbeckwi/My Documents/Org/Tasks.org"
         "ReQall entries")
        ("Reader Starred"
         "http://www.google.com/reader/public/atom/user%2F18264969865616704417%2Fstate%2Fcom.google%2Fstarred"
         "c:/Documents and Settings/bnbeckwi/My Documents/Org/Refile.org"
         "Google Reader Starred Items"
         :template "\n* %h\n  %U\n#+BEGIN_HTML \n %description\n#+END_HTML \n  %a\n"
         :parse-feed org-feed-parse-atom-feed
         :parse-entry org-feed-parse-atom-entry)
        ("Foxmarks"
         "http://share.xmarks.com/folder/rss/D5rnYUy6cN"
         "c:/Documents and Settings/bnbeckwi/My Documents/Org/Refile.org"
         "Bookmarks to Sort"
         :formatter bnb/format-xmarks-entries
         )
        ))

;; Update from home (no proxy set)
(defun bnb/org-update-from-home ()
  (interactive)
  (let (( url-proxy-services
          '(("no_proxy" . ".*")) ))
    (org-feed-update-all)))

;; Format Xmarks rss entries properly
;; mainly, this is to ensure that the entries are tagged.
(defun bnb/format-xmarks-entries (entries)
  (let (template dlines tmp indent time
                 v-h v-t v-T v-u v-U v-a
                 (desc (or (plist-get entry :description) "???"))
                 )
    (setq template "* %h      %g\n  %U\n  %description\n  %a\n"
          dlines (org-split-string desc
                                   "\n")
          v-g (concat ":Bookmark:"
                      (replace-regexp-in-string
                       "[\s-]" "_"
                       (replace-regexp-in-string
                        ",\s*" ":"
                        (if (string-match
                             "Tags: \\(.*\\)"
                             desc)
                            (concat (match-string-no-properties 1 desc) ":")
                          ""
                          ))))
          v-h (or (plist-get entry :title) (car dlines) "???"  )
          time (or (if (plist-get entry :pubDate)
                       (org-read-date t t (plist-get entry :pubDate)))
                   (current-time))
          v-t (format-time-string (org-time-stamp-format nil nil) time)
          v-T (format-time-string (org-time-stamp-format t   nil) time)
          v-u (format-time-string (org-time-stamp-format nil   t) time)
          v-U (format-time-string (org-time-stamp-format t     t) time)
          v-a (if (setq tmp (or (and (plist-get entry :guid-permalink)
                                     (plist-get entry :guid))
                                (plist-get entry :link)))
                  (concat "[[" tmp "]]\n")
                ""))
    (with-temp-buffer
      (insert template)
      (goto-char (point-min))
      (while (re-search-forward "%\\([a-zA-Z]+\\)" nil t)
        (setq name (match-string 1))
        (cond
         ((member name '("h" "t" "T" "u" "U" "a" "g"))
          (replace-match (symbol-value (intern (concat "v-" name))) t t ))
         ((setq tmp (plist-get entry (intern (concat ":" name))))
          (save-excursion
            (save-match-data
              (beginning-of-line 1)
              (when (looking-at (concat "^\\([ \t]*\\)%" name "[ \t]*$"))
                (setq tmp (org-feed-make-indented-block
                           tmp (org-get-indentation))))))
          (replace-match tmp t t))))
      (buffer-string))))
    

;; Anything integration
(defvar org-remember-anything
  '((name . "Org Remember")
    (candidates . (lambda () (mapcar 'car org-remember-templates)))
    (action . (lambda (name)
                (let* ((orig-template org-remember-templates)
                       (org-remember-templates
                        (list (assoc name orig-template))))
                  (call-interactively 'org-remember))))))


;; Org-Babel setup
(require 'ob)
(require 'ob-lob)
(require 'ob-sqlite)
(require 'ob-sh)
(setq org-babel-sh-command "cmd /k")

(provide 'org-config)