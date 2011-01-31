(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "M-SPC") 'anything)
(setq anything-sources
      (list anything-c-source-buffers
	    anything-c-source-recentf
            anything-c-source-kill-ring
	    anything-c-source-org-headline
            ))

(provide 'bnb-anything-config)