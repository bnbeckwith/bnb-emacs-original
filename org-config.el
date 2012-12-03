;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode


;;;;;
;; ORG INSTALLATION
;;;;;

	     ;; .org .org_archive and .org.gpg files are org-mode files
(add-to-list 'auto-mode-alist '("\\.org\\(.gpg|_archive\\)?$" . org-mode))

; Install org-mode
(require 'org)
(require 'org-protocol)

;; setup some hooks.
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; Keybindings
	    (local-set-key "\M-I" 'org-toggle-inline-images)
	    ))
(add-hook 'bnb/really-kill-emacs-hooks 'org-save-all-org-buffers 'append)

;; Setup some global keys
(global-set-key (kbd "C-c C-t") 'orgtbl-mode)

;;;;;
;; ORG MISCELLANEOUS
;;;;;

;; Save all of my org-buffers every hour.
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Setup applications to use to open links. This lets the sytem (OS)
;; decide which application is necesary for the given filename.
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

;; Do NOT use odd-levels only (nil is the default, but let's be
;; explicit)
(setq org-odd-levels-only nil)

;; Do NOT put empty lines between collapsed trees
(setq org-cycle-separator-lines 0)

;; Do NOT reverse note order
(setq org-reverse-note-order nil)

;; Insert new heading after the current subtree.
(setq org-insert-heading-respect-content t)

;; Startup with content showing
(setq org-startup-folded 'content)

;; Hidestars globally
(setq org-hide-leading-stars t)

;; Fontify the SRC blocks natively
(setq org-src-fontify-natively t)

;; DELETE THIS SECTION
;; ;; Google Weather
;; (add-to-list 'load-path (concat bnb-elisp-dir "google-weather-el/"))
;; ; I had to turn off https to have this work at work.
;; (setq google-weather-use-https nil)
;; (require 'org-google-weather)

;;;;
;; ORG KEYS
;;;;
; Set up some keystrokes
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> i") 'org-clock-in)
(global-set-key (kbd "<f9> o") 'org-clock-out)
(global-set-key (kbd "<f11> i") 'org-clock-in)
(global-set-key (kbd "<f11> g") 'org-clock-goto)
;; TODO -- this should be moved elsewhere.
(global-set-key (kbd "<f11> s") 'flyspell-buffer)


;; Speed commands are used when on the * of a given headline.  If
;; these are forgotten, just press '?' as a speed-command to bring up
;; the cheat-sheet.
(setq org-use-speed-commands t)
(setq org-speed-commands-user (quote (("0" . delete-window)
				      ("1" . delete-other-windows)
				      ("2" . split-window-vertically)
				      ("3" . split-window-horizontally)
				      ("h" . hide-other)
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

;; Manage the global tag list
(setq org-tag-alist '(("PROJECT" . ?p)))


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
               ("SOMEDAY" . t))
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
  "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
; turn off logging
  (let (org-log-done org-log-states)
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; Add some hooks
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Turn off task priorities
(setq org-enable-priority-commands nil)


;;;;;
;; ORG OUTLOOK
;;;;;

;; Only use this on windows
;; (if (eq window-system 'w32)
;;     (require 'org-outlook))


;;;;;
;; ORG CAPTURE
;;;;;

;; Capture Templates
(setq org-capture-templates
      '(("t" "Todo" entry
	 (file "~/Documents/Org/Refile.org")
	 "* TODO %?\n  %U\n" :clock-in t :clock-resume t)
	("r" "todo (Remember location)" entry
	 (file "~/Documents/Org/Refile.org")
	 "* TODO %?\n  %U\n  %a" :clock-in t :clock-resume t)
	("n" "Note" entry
	 (file "~/Documents/Org/Refile.org")
	 "* %?                                                                            :NOTE:\n  %U\n  %a\n  :CLOCK:\n  :END:")
	("w" "Weekly Report" entry
	 (file+headline "~/Documents/Org/WeeklyReports.org" "2012")
	 "* %(bnb/workweek-string) \n %? \n %i")
	("o" "org-outlook" entry
	 (file "~/Documents/Org/Refile.org")
	 "* TODO EMAIL: %:title (%:sender)
  %c

  %?" :clock-in t :clock-resume t)))

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

;; Agenda clock report parameters (no links, 2 levels deep, skip 0 clocking)
(setq org-agenda-clockreport-parameter-plist (quote (:link nil :maxlevel 2 :fileskip0 t)))

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
              ("n" "Next" tags-todo "-WAITING-CANCELED/!NEXT"
               ((org-agenda-overriding-header "Next Tasks")))
              ("A" "Tasks to be Archived" tags "LEVEL=2-REFILE/DONE|CANCELED"
               ((org-agenda-overriding-header "Tasks to Archive")))
	      ("u" "Upcoming tasks" tags "+SCHEDULED<=\"<+1w>\"-TODO=\"DONE\"|+DEADLINE<=\"<+1w>\"-TODO=\"DONE\""
	       ((org-agenda-overriding-header "Upcoming tasks")))
	      ("U" todo "TODO"
	       ((org-agenda-skip-function
		 (lambda nil
		   (org-agenda-skip-entry-if (quote scheduled) (quote deadline))))))
	      ("P" "Personal Tasks Todo" ;tags-todo "-DONE-CANCELED-SOMEDAY"
	       ((tags-todo "+IGNORE"
			   ((org-agenda-overriding-header "\n== Upcoming Items ==\n")))
		(agenda "" ((org-agenda-span 14)
	       		    (org-agenda-start-on-weekday nil)
			    (org-agenda-prefix-format "[ ] %T:\t")
	       		    (org-agenda-repeating-timestamp-show-all t)
	       		    (org-deadline-warning-days 7)))
		;; (tags-todo "+IGNORE"
		;; 	   ((org-agenda-overriding-header "\n** Due Today **\n")))
	       	;; (agenda "" ((org-agenda-span 1)
	       	;; 	    (org-agenda-todo-keyword-format "[ ]")
	       	;; 	    (org-agenda-scheduled-leaders '("" ""))
	       	;; 	    (org-agenda-prefix-format "%t%s")))
	       	(tags-todo "-DRB-SOMEDAY-REFERENCE-BNBECKWITH"
			   ((org-agenda-prefix-format "[ ] %T:\t")
			    (org-agenda-sorting-strategy '(tag-up priority-down))
			    (org-agenda-todo-keyword-format "")
			    (org-agenda-todo-ignore-with-date nil)
			    (org-agenda-todo-ignore-scheduled nil)
			    (org-agenda-todo-ignore-deadlines nil)
			    (org-agenda-overriding-header "\nTasks by Context\n------------------\n"))))
	       ((ps-number-of-columns 2)
		(ps-landscape-mode t)
		(org-agenda-with-colors nil)
		(org-agenda-compact-blocks t)
		(org-agenda-remove-tags t)
		(ps-paper-type 'a4))
	       ("~/TODO.pdf" "~/TODO.ps"))
	      ("S" "Scoreless" tags-todo "+Score<1"
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-overriding-header "Scoreless Tasks")))
              ("h" "Habits" tags "STYLE=\"habit\""
               ((org-agenda-todo-ignore-with-date nil)
                (org-agenda-todo-ignore-scheduled nil)
                (org-agenda-todo-ignore-deadlines nil)
                (org-agenda-overriding-header "Habits")))
              ("#" "Stuck Projects" tags-todo "LEVEL=2-REFILE+PROJECT|LEVEL=1+REFILE/!-DONE-CANCELED"
               ((org-agenda-skip-function 'bh/skip-non-stuck-projects)
                (org-agenda-overriding-header "Stuck Projects")))
	      ("z" "Agenda (including Personal Files)" agenda ""
	       ((org-agenda-files (list "~/Documents/Personal/Org/"))))
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
(setq org-export-with-LaTeX-fragments "dvipng")

;; Minted (for latex) settings
(setq org-export-latex-listings 'minted)
(setq org-export-latex-custom-lang-environments
      '(
	(emacs-lisp "common-lispcode")
	))
(setq org-export-latex-minted-options
      '(
	))
(setq org-latex-to-pdf-process
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Remove HTML postamble
(setq org-export-html-postamble nil)

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
;;(setq org-clock-in-switch-to-state (quote bnb/clock-in-to-next))
;;(setq org-clock-in-switch-to-state "NEXT")
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

;; ;; Clock into default task or show a list of possible default tasks
;; (defun bnb/clock-in ()
;;   (interactive)
;;   (if (marker-buffer org-clock-default-task)
;;       (unless (org-clock-is-active)
;; 	(bnb/clock-in-default-task))
;;     (org-agenda nil "c")))

;; ;; Clock out if clock is active
;; (defun bnb/clock-out ()
;;   (interactive)
;;   (when (org-clock-is-active)
;;     (org-clock-out)))

;; ;; Clock into the default task
;; (defun bnb/clock-in-default-task ()
;;   (save-excursion
;;     (org-with-point-at org-clock-default-task
;;       (org-clock-in))))

;; ;; Comment inline
;; (defun bnb/clock-in-to-next (kw)
;;   "Switch task from TODO to NEXT when clocking in.
;; Skips remember tasks and tasks with subtasks"
;;   (if (and (string-equal kw "TODO")
;; 	   (not (and (boundp 'org-capture-mode) org-capture-mode)))
;;       (let ((subtree-end (save-excursion (org-end-of-subtree t)))
;; 	    (has-subtask nil))
;; 	(save-excursion
;; 	  (forward-line 1)
;; 	  (while (and (not has-subtask)
;; 		      (< (point) subtree-end)
;; 		      (re-search-forward "^\*+ " subtree-end t))
;; 	    (when (member (org-get-todo-state) org-not-done-keywords)
;; 	      (setq has-subtask t))))
;; 	(when (not has-subtask)
;; 	  "NEXT"))))

(setq org-time-stamp-rounding-minutes (quote (1 15)))

;; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format
      "%80ITEM(Task) %5Score{+} %10Effort(Effort){:} %10CLOCKSUM")
;; Effort_ALL is set below in the ORG HABIT section.

;;;;;
;; ORG MODULES
;;;;;
(setq org-modules
      (quote
       (org-bbdb
	org-bibtex
	org-crypt
	org-gnus
	org-id
	org-info
	org-jsinfo
	org-habit
	org-inlinetask
	org-irc
	org-protocol
	org-w3m
	org-bookmark
	org-calc)))

;;;;;
;; ORG BABEL
;;;;;

;; Initialize the following languages by default
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (ditaa . t)
   (dot . t)
   (latex . t)
   (python . t)
   (perl . t)
   (R . t)
   (C . t)
   (sqlite . t)))


(setq org-babel-R-command "C:/Progra~1/R/R-2.15.1/bin/R --slave --no-save")

;;;;;
;; ORG CRYPT
;;;;;
; To encrypt a block, just add a "crypt" tag and save the file.
; To decrypt, use org-decrypt-entry or org-decrypt-entries
(require 'org-crypt)
; This was included in the org-modules
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; Key to use for encryption
(setq org-crypt-key "F0F919A1")

;;;;;
;; ORG HABIT
;;;;;
(setq org-global-properties (quote (("STYLE_ALL" . "habit")
				    ("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00")
				    ("Score_ALL" . "10 5 2 1"))))
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
;; TODO -- are these steps still necessary?
;; (require 'iimage)
;; ;; Fix the links to like windows paths.
;; (add-to-list 'iimage-mode-image-regex-alist
;; 	     (cons (concat "\\[\\[file:[a-zA-Z]:?\\(~?"
;; 			   (concat "[-+./_0-9a-zA-Z%]+\\."
;; 				   (regexp-opt (nconc (mapcar #'upcase
;; 							      image-file-name-extensions)
;; 						      image-file-name-extensions)
;; 					       t))
;;  			   "\\)\\]")  1))

;; (defadvice iimage-locate-file (before bnb/reinsert-spaces activate)
;;   "Change %20 in the filename back into proper spaces"
;;   (ad-set-arg 0 (replace-regexp-in-string "%20" " " (ad-get-arg 0))))


;;;;;
;; ORG FEEDS
;;;;;
(require 'org-feed)

;; Setup feeds for ReQall
(setq org-feed-alist
      '(("ReQall"
         "http://www.reqall.com/user/feeds/rss/ef1f7fd093ca0a7d98ba5758d64b00775c47ccf3"
         "~/Documents/Org/Refile.org"
         "Tasks")
        ))

;; Update from home (no proxy set)
(defun bnb/org-update-from-home ()
  (interactive)
  (let (( url-proxy-services
          '(("no_proxy" . ".*")) ))
    (org-feed-update-all)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Org-publish for bnbeckwith.com
;; (setq org-publish-project-alist
;;       '(("org-bnb"  :base-directory "~/Documents/Personal/Projects/bnbeckwith.com/org"
;; 	            :base-extension "org"
;; 		    :publishing-directory "~/Documents/Personal/Projects/bnbeckwith.com/site"
;; 		    :recursive t
;; 		    :publishing-function org-publish-org-to-html
;; 		    :headline-levels 4
;; 		    :html-extension "html"
;; 		    :style "
;; <link rel=\"canonical\" href=\"http://sandbox.bnbeckwith.com/\" />

;;   <!-- /ht Andy Clarke - http://front.ie/lkCwyf -->
;;   <meta http-equiv=\"cleartype\" content=\"on\" />
;;   <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge,chrome=1\" />
;;   <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />

;;   <link rel=\"shortcut icon\" href=\"/assets/ico/favicon.ico\" />
;;   <link rel=\"apple-touch-icon\" href=\"/assets/ico/apple-touch-icon.png\" />

;;   <!-- /ht Jeremy Keith - http://front.ie/mLXiaS -->
;;   <link rel=\"stylesheet\" href=\"/assets/css/global.css\" media=\"all\" />
;;   <link rel=\"stylesheet\" href=\"/assets/css/custom.css\" type=\"text/css\" media=\"screen\" />
;;   <link rel=\"stylesheet\" href=\"/assets/css/layout.css\" media=\"all and (min-width: 33.236em)\" \>
;;   <!-- 30em + (1.618em * 2) = 33.236em / Eliminates potential of horizontal scrolling in most cases -->

;;   <!--[if (lt IE 9) & (!IEMobile)]>
;;       <link rel=\"stylesheet\" href=\"/assets/css/layout.css\" media=\"all\">
;;       <![endif]-->

;;   <script src=\"/assets/js/libs/modernizr-1.7.min.js\"></script>
;;   <link href=\"\" rel=\"alternate\" title=\"\" type=\"application/atom+xml\" />
;; "
;; 		    :html-preamble "</div>
;; <div id=\"container\" class=\"cf\">
;;     <div id=\"top\">
;;   <header>
;;     <h1> <span id=\"title\">bnbeckwith</span> </h1>
;;     <ul id=\"topmenu\">
;;       <li><a href=\"/index.html\">/Blog</a></li>
;;       <li><a href=\"/categories.html\">/Categories</a></li>
;;       <li><a href=\"/code.html\">/Code</a></li>
;;       <li><a href=\"/about.html\">/About</a></li>
;;       <li>
;; 	<form method=\"GET\" action=\"https://duckduckgo.com/\" onSubmit=\"document.getElementById('sitesearch').value+=' site:bnbeckwith.com';\">
;; 	  <input name=\"q\" type=\"text\" placeholder=\"/Search\" id=\"sitesearch\" />
;;       </form></li>
;;     </ul>
;;   </header>
;; </div>
;;   <div id=\"main\" role=\"main\">
;;    <div id=\"ignore\">
;; "
;; 		    :html-postamble
;; "</div></div><footer>&copy; Benjamin Beckwith 2009-2012</footer></div>"
;; 		    :auto-sitemap t
;; 		    :style-include-default nil
;; 		    :style-include-scripts nil)
;; 	("static-bnb" :base-directory "~/Documents/Personal/Projects/bnbeckwith.com/org"
;; 	              :base-extension "ico\\|css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
;; 		      :publishing-directory "~/Documents/Personal/Projects/bnbeckwith.com/site"
;; 		      :recursive: t
;; 		      :publishing-function org-publish-attachment)
;; 	("bnb" :components ("org-bnb" "static-bnb"))))

(provide 'org-config)
