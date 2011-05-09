;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My (bnbeckwith) specific customization file

(require 'cl)
(defvar *emacs-load-start* (current-time))

(setq bnb-elisp-dir "~/elisp/")
(dolist (f (directory-files bnb-elisp-dir))
  (let ((name (concat bnb-elisp-dir f)))
    (when (and (file-directory-p name)
	       (not (equal f ".."))
	       (not (equal f "." )))
      (add-to-list 'load-path name))))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)


(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

; Remove menu bar
(menu-bar-mode 0)
; Remove tool bar
(tool-bar-mode 0)

; Set the title of the frame
(setq frame-title-format '("%b - %F"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving history in emacs
(setq savehist-additional-variables        ;; also save
      '(search-ring regexp-search-ring)    ;; search entries
      savehist-file "~/.emacs.d/savehist") ;; A place to store the
                                           ;; history
(savehist-mode t)                          ;; Turn on the mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BNB specific helper code
(require 'bnb-helpers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired config
(require 'dired-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO Settings
(require 'ido-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EShell settings
(require 'eshell-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoHotKey Mode
(require 'ahk-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode
(require 'org-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org2blog
;; FIX FIX FIX - Find a way to make this optional
;;(require 'org2blog-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anything mode
(require 'bnb-anything-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auctex
(require 'auctex-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; WC-mode
(require 'wcmode-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writegood mode
(require 'writegood-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlighting
;(global-font-lock-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Bookmarks
;;Auto-save bookmarks
(setq bookmark-save-flag 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; YASnippet mode
;;(require 'yas-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ledger mode
;;(require 'ledger-config)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wikipedia (MediaWiki) mode
(autoload 'wikipedia-mode "wikipedia-mode.el"
  "Major mode for editing documents in Wikipedia markup." t)
(add-to-list 'auto-mode-alist
             '("wiki\\.pdx\\.intel\\.com" . wikipedia-mode))
;(add-hook 'wikipedia-mode-hook 'orgtbl-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Show Paren mode
(show-paren-mode t)
;(setq show-paren-style 'parenthesis)
;(set-face-attribute 'show-paren-match-face nil :weight 'bold :underline nil :overline nil :slant 'normal)
;(set-face-background 'show-paren-mismatch-face "red")
;(set-face-attribute 'show-paren-mismatch-face nil :weight 'bold :underline nil :overline nil :slant 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cperl-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EasyPG
(require 'epa)
(epa-file-enable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To Consider:
;;  - par-edit (lisp editing)
;;  - hl-line
;;  - autopair
;;  - http://github.com/bbatsov/emacs


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs customization from built-in system
(setq custom-file "~/.emacs.d/custom.el")
(load-file custom-file)


(message "My bnb-emacs loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
					(- (+ hi lo) (+ (first *emacs-load-start*)
							(second *emacs-load-start*)))))

(provide 'bnb-init)
