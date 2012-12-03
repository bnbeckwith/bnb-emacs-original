
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping emacs running even when "exiting"
;; http://emacs-fu.blogspot.com/2009/03/windows-and-daemons.html 
(defun bnb/exit ()
  (interactive)
;  Check for a server-buffer before closing the server-buffer
  (if (functionp 'server-edit)
      (server-edit))
  (iconify-frame))

;; Don't exit on windows with a keystroke.
(when (eq window-system 'w32)
  (global-set-key (kbd "C-x C-c") 'bnb/exit))

(defvar bnb/really-kill-emacs-hooks)

(defun bnb/really-kill-emacs ()
  (interactive)
  (setq bnb/really-kill-emacs t)
  (run-hooks 'bnb/really-kill-emacs-hooks)
  (kill-emacs))

(defvar bnb/really-kill-emacs nil)

(when (eq system-type 'windows-nt)
  (defadvice kill-emacs (around bnb/really-exit activate)
    "Only kill emacs if a prefix is set"
    (if bnb/really-kill-emacs
	ad-do-it)
    (iconify-frame)))
  
(defun bnb/workweek ()
  (interactive)
  (let* ((now (current-time))
	 (weeks (string-to-number
		 (format-time-string "%W" now)))
	 (days (time-to-day-in-year now))
	 (daynum (string-to-number
		  (format-time-string "%w" now)))
	 (left (% days 7)))
    (if (and (= 0 daynum) (= left 0))
	weeks
      (+ 1 weeks))))
    
(defun bnb/workweek-string ()
  (interactive)
  (concat "WW" 
	  (number-to-string
	   (bnb/workweek))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Take a screenshot
(when (eq window-system 'w32)
  (global-set-key (kbd "<scroll>") 'bnb/screenshot)

  (defun bnb/screenshot ()
    "Take a screenshot and copy the filename to clipboard"
    (interactive)
    (if (eq system-type 'windows-nt)
	(call-process "C:/Program Files (x86)/ScreenshotCaptor/ScreenshotCaptor.exe" nil nil nil "-capture" "activewindow" "-show")
      (setq filename 
	    (concat (make-temp-name (file-name-directory (buffer-file-name))) ".jpg"))
      (call-process "import" nil nil nil filename)
      (kill-new filename)))
)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle full-screen emacs
(defun bnb/fullscreen (&optional f)
  (interactive)
  (set-frame-parameter f 'fullscreen
		       (if (frame-parameter f 'fullscreen) nil 'fullboth)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO markers in sidebar
(defun bnb/annotate-todo ()
  "Put triangle markers in fringe at TODO items"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "TODO" nil t)
      (let ((overlay (make-overlay (- (point) 5) (point))))
	(overlay-put overlay 'before-string 
		     (propertize (format "A")
				 'display '(left-fringe right-triangle)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hippie Expand
(global-set-key (kbd "M-/") 'hippie-expand)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix docview on windows
(if (eq window-system 'w32)
    (setq doc-view-ghostscript-program
	  "C:/Program Files (x86)/gs/gs9.02/bin/gswin32c.exe"))

(provide 'bnb-helpers)
