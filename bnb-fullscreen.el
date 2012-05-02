; https://bitbucket.org/alexander_manenko/emacs-fullscreen-win32/wiki/Home
; Binary emacs_fullscreen.exe is in my ~/bin 
; Now I just have to bind a command.

(defun bnb/toggle-full-screen () (interactive) (shell-command "emacs_fullscreen.exe"))
(global-set-key (kbd "<f11> <f11>") 'bnb/toggle-full-screen)

(provide 'bnb-fullscreen)