
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keeping emacs running even when "exiting"
;; http://emacs-fu.blogspot.com/2009/03/windows-and-daemons.html 
(defun bnb/exit ()
  (interactive)
;  Check for a server-buffer before closing the server-buffer
  (if server-clients
      (server-edit))
  (make-frame-invisible nil t))
(global-set-key (kbd "C-x C-c") 'bnb/exit)

(defvar bnb/really-kill-emacs-hooks)

(defun bnb/really-kill-emacs ()
  (interactive)
  (setq bnb/really-kill-emacs t)
  (run-hooks 'bnb/really-kill-emacs-hooks)
  (kill-emacs))

(defvar bnb/really-kill-emacs nil)
(defadvice kill-emacs (around bnb/really-exit activate)
  "Only kill emacs if a prefix is set"
  (if bnb/really-kill-emacs
      ad-do-it)
    (bnb/exit))

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

(provide 'bnb-helpers)