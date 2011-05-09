;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDO settings



;; C-b 
;; ** Reverts to the old switch-buffer completion engine 
;; ** Buffers
;; 
;; C-f 
;; ** Reverts to the old find-file completion engine 
;; ** Files
;; 
;; C-d 
;; ** Opens a dired buffer in the current directory 
;; ** Dirs / Files
;; 
;; C-a 
;; ** Toggles showing ignored files (see ido-ignore-files) 
;; ** Files / Buffers
;; 
;; C-c 
;; ** Toggles if searching of buffer and file names should ignore case. 
;;    (see ido-case-fold) 
;; ** Dirs / Files / Buffers
;; 
;; TAB 
;; ** Attempt to complete the input like the normal completing read 
;;    functionality 
;; ** Dirs / Files / Buffers
;; 
;; C-p 
;; ** Toggles prefix matching; when it's on the input will only match 
;;    the beginning of a filename instead of any part of it. 
;; ** Files
;; 
;; C-s / C-r 
;; ** Moves to the next and previous match, respectively 
;; ** All
;; 
;; C-t 
;; ** Toggles matching by Emacs regular expression. 
;; ** All
;; 
;; Backspace 
;; ** Deletes characters as usual or goes up one directory if it makes sense to
;;    do so. 
;; ** All (but functionality varies)
;; 
;; C-SPC / C-@ 
;; ** Restricts the completion list to anything that matches your current input. 
;; ** All
;; 
;; // 
;; ** Like most *nix shells two forward slashes in a path means "ignore the 
;;    preceding path, and go back to the top-most directory". Works the same in 
;;    Ido but it's more interactive: it will go to the root / (or the root of 
;;    the current drive in Windows) 
;; ** Files
;; 
;; ~/ 
;; ** Jumps to the home directory. On Windows this would be typically be 
;;    %USERPROFILE% or %HOME%, if it is defined. 
;; ** Files / Dirs
;; 
;; M-d 
;; ** Searches for the input in all sub-directories to the directory you're in. 
;; ** Files
;; 
;; C-k 
;; ** Kills the currently focused buffer or deletes the file depending on the mode. 
;; ** Files / Buffers
;; 
;; M-m 
;; ** Creates a new sub-directory to the directory you're in 
;; ** Files
;; 


;; M-n / M-p 	
;; ** Cycles through the next or previous work directories
;;
;; M-k 	
;; ** Kills (removes) the active work directory from the list.
;; 
;; M-s 	M-s 
;; ** forces ido to search the list of work directories for the current input.



;; General settings
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;; Favor some file extensions
(setq ido-file-extensions-order '(".org" ".tex" ".el" ".txt" ".c" ".cpp" ".h" ".lisp"))

(ido-mode 1)
(provide 'ido-config)