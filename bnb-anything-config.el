(require 'anything-config)
(require 'anything-match-plugin)
(global-set-key (kbd "M-SPC") 'anything)
(setq anything-sources
      (list anything-c-source-buffers
	    anything-c-source-recentf
            anything-c-source-complex-command-history
            anything-c-source-kill-ring
            anything-c-source-occur
	    anything-c-source-org-headline
            org-remember-anything
	    anything-c-source-lacarte
            ))

(provide 'bnb-anything-config)