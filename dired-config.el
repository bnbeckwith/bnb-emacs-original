;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired+ instead of dired

(require 'dired+)

; enable 'a' command that replaces the current dired buffer
(put 'dired-find-alternate-file 'disabled nil)

(provide 'dired-config)
