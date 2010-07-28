;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode


;;;;;
;; ORG INSTALLATION
;;;;;
(setq bnb-org-location (concat bnb-elisp-dir "org-mode/"))

;; Tell emacs where to find it and when to start it
(add-to-list 'load-path (expand-file-name (concat bnb-org-location "lisp/")) 'prepend)
(add-to-list 'load-path (expand-file-name (concat bnb-org-location "contrib/lisp")) 'prepend)
	     ;; .org .org_archive and .org.gpg files are org-mode files
(add-to-list 'auto-mode-alist '("\\.org\\(.gpg|_archive\\)?$" . org-mode))

; Install org-mode
(require 'org-install)
(require 'org-protocol)

;; setup some hooks.
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; Keybindings
	    (local-set-key "\M-I" 'org-toggle-iimage-in-org)
	    ;; Turn on flyspell mode
					;(flyspell-mode 1)
	    ))
(add-hook 'bnb/really-kill-emacs-hooks 'org-save-all-org-buffers 'append)

;;;;;
;; ORG MISCELLANEOUS
;;;;;

;; Add the installed info directory to the Info list
(add-to-list 'Info-default-directory-list (concat bnb-org-location "doc/org"))

;; Save all of my org-buffers every hour.
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Setup applications to use to open links.
(setq org-file-apps 
      (quote ((auto-mode . emacs) 
	      ("\\.x?html?\\'" . default) 
	      ("\\.pdf\\'" . default) 
	      ("\\.mm\\'" . default))))

;; Use the same interface as REFILE when using org-goto
(setq org-goto-interface (quote outline-path-completion))

;; supersize the C-k on headlines
(setq org-special-ctrl-k t)

;; Use IDO mode for selections
(setq org-completion-use-ido t)
;; Recommended to turn this off when IDO is enabled.
(setq org-outline-path-complete-in-steps nil)

;; Auto-revert mode.  This will be useful once the orgfiles are in git
;; repos.
(setq global-auto-revert-mode t)

;; When should org leave a blank line before an item?
(setq org-blank-before-new-entry (quote ((heading)
 					 (plain-list-item))))

;; Hide the leading stars so that we aren't seeing stars.
(setq org-hide-leading-stars t)

;; Do NOT use odd-levels only (this is the default, but let's be
;; explicit)
(setq org-odd-levels-only nil)

;; Do NOT put empty lines between collapsed trees
(setq org-cycle-separator-lines 0)

;; Do NOT reverse note order
(setq org-reverse-note-order nil)

;; Insert new heading after the current subtree.
(setq org-insert-heading-respect-content t)

;;;;
;; ORG KEYS
;;;;
; Set up some keystrokes
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)
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


;; Speed commands are used when on the * of a given headline.  If
;; these are forgotten, just press '?' as a speed-command to bring up
;; the cheat-sheet.
(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
				      ("1" . delete-other-windows)
				      ("2" . split-window-vertically)
				      ("3" . split-window-horizontally)
				      ("h" . hide-other)
				      ("k" . org-kill-node-or-show-branches)
				      ("R" . org-reveal)
				      ("s" . org-save-all-org-buffers)
				      ("z" . org-add-note)
				      ("N" . org-narrow-to-subtree)
				      ("W" . widen))))

;;;;
;; ORG TODO
;;;;

;; Setup the TODO keyword sequences.
(setq org-todo-keywords 
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
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

;; Settings for enforcing TODO constraints
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)

; Set up the summary to use.
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
; turn off logging
  (let (org-log-done org-log-states) 
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; Add some hooks
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;;;;;
;; ORG CAPTURE
;;;;;

;; Capture Templates
(setq org-capture-templates
      '(("t" "todo" entry
	 (file "~/Documents/Org/Refile.org")
	 "* TODO %?\n  %U\n  %a" :clock-in t :clock-resume t)
	("n" "note" entry
	 (file "~/Documents/Org/Refile.org")
	 "* %?                                                                            :NOTE:\n  %U\n  %a\n  :CLOCK:\n  :END:")
	("w" "Wiki Award" table-line
	 (file+headline "~/Documents/Org/Wiki.org" "Wiki Award Winners")
	 "|%(bnb/workweek-string)|%^{Winner}|%^{Comment}|%^{Award Sent}|" :prepend t )
	("p" "org-protocol" entry
	 (file "~/Documents/Org/Refile.org")
	 "* TODO Review %c\n  %U" :immediate-finish t)))


;;;;;
;; ORG REFILE
;;;;;

;; Cache the targets for refiling.  This is the best option for me as
;; I am usually refiling into separate files at a Tasks headline.
;; Because this rarely changes, caching should be fine.
(setq org-refile-use-cache t)
;; Allow refiling into any of the agenda files up to three levels
;; deep.  Also allow refiling in the same file up to 5 levels deep.
(setq org-refile-targets (quote 
			  ((org-agenda-files :maxlevel . 3)
			   (nil :maxlevel . 5))))
;; Filenames *must* be first for refiling.
(setq org-refile-use-outline-path 'file)


;;;;;
;; ORG AGENDA
;;;;;

;; Display any inherited tags in the agenda line
(setq org-agenda-show-inherited-tags t)

;; Show the log for items that have been clocked in the day.
;; C-u l to display full log in agenda.
(setq org-agenda-log-mode-items '(clock))

;; Agenda clock report parameters (no links, 2 levels deep)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2)))

;; Only show a time grid on today if there is a scheduled task.
(setq org-agenda-time-grid 
      (quote ((daily today require-timed) 
	      "----------------" 
	      (800 1000 1200 1400 1600 1800 2000))))

;; Setup the various Agenda views
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

;; Define Stuck projects for the Agenda view
(setq org-stuck-projects 
      (quote 
       ("+LEVEL=2/-DONE" ("TODO" "NEXT" "NEXTACTION") ("REFERENCE" "ARCHIVE") "")))

;;;;;
;; Org EXPORT
;;;;;

;; Put the images inline with an <img> tag.
(setq org-export-html-inline-images t)

;; Convert any LaTeX fragments to images for export.
(setq org-export-with-LaTeX-fragments t)


;;;;;
;; ORG LOG
;;;;;

;; Set a note when logging an item done.
(setq org-log-done (quote note))
(setq org-log-into-drawer t)
;; Separate drawers for clocking and logs
(setq org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK"))


;;;;;
;; ORG CLOCKING
;;;;;

;; Resume clocking tasks when emacs is restarted.
(org-clock-persistence-insinuate)
;; Setup clocking history
(setq org-clock-history-length 28)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Change state to NEXT when clocking in a TODO item
(setq org-clock-in-switch-to-state (quote bnb/clock-in-to-next))
;; Clock into the CLOCK drawer
(setq org-clock-into-drawer "CLOCK")
;; Remove any 0 time clocks.
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out if the item is marked done
(setq org-clock-out-when-done t)
;; Keep clock history across emacs sessions.
(setq org-clock-persist 'history)
;; Automatically resolve open clocks.
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
;; Include the clocking task in a clock report
(setq org-clock-report-include-clocking-task t)

;; Clock into default task or show a list of possible default tasks
(defun bnb/clock-in ()
  (interactive)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
	(bnb/clock-in-default-task))
    (org-agenda nil "c")))

;; Clock out if clock is active
(defun bnb/clock-out ()
  (interactive)
  (when (org-clock-is-active)
    (org-clock-out)))

;; Clock into the default task
(defun bnb/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

;; Comment inline
(defun bnb/clock-in-to-next (kw)
  "Switch task from TODO to NEXT when clocking in.
Skips remember tasks and tasks with subtasks"
  (if (and (string-equal kw "TODO")
	   (not org-capture-mode))
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

(setq org-time-stamp-rounding-minutes (quote (1 15)))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format 
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
;; Effort_ALL is set below in the ORG HABIT section.

;;;;;
;; ORG MODULES
;;;;;
(setq org-modules (quote (org-bbdb org-bibtex org-crypt org-gnus org-id org-info org-jsinfo org-habit org-inlinetask org-irc org-protocol org-w3m org-bookmark)))

;;;;;
;; ORG HABIT
;;;;;
(setq org-global-properties (quote (("STYLE_ALL" . "habit")
				    ("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))
(setq org-habit-graph-column 50)



;;;;;
;; ORG PROTOCOL
;;;;;
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

;;;;;
;; ORG IIMAGE
;;;;;

(require 'iimage)
;; Fix the links to like windows paths.
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


;;;;;
;; ORG FEEDS
;;;;;
(require 'org-feed)

;; Setup feeds for ReQall, Reader and Foxmarks.
(setq org-feed-alist
      '(("ReQall"
         "http://www.reqall.com/user/feeds/rss/ef1f7fd093ca0a7d98ba5758d64b00775c47ccf3"
         "c:/Users/bnbeckwi/Documents/Org/Work.org"
         "Tasks")
        ("Reader Starred"
         "http://www.google.com/reader/public/atom/user%2F18264969865616704417%2Fstate%2Fcom.google%2Fstarred"
         "c:/Users/bnbeckwi/Documents/Org/GoogleReader.org"
         "Google Reader Starred Items"
         :template "\n* %h\n  %U\n#+BEGIN_HTML \n %description\n#+END_HTML \n  %a\n"
         :parse-feed org-feed-parse-atom-feed
         :parse-entry org-feed-parse-atom-entry)
        ("Foxmarks"
         "http://share.xmarks.com/folder/rss/D5rnYUy6cN"
         "c:/Users/bnbeckwi/Documents/Org/Bookmarks.org"
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
    

(provide 'org-config)