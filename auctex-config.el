;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX mode
;; Commented for faster load-times
(load "auctex.el" t)
(load "preview-latex.el" nil t t)
(require 'tex-mik)

;; Potential settings
; (setq TeX-view-style (quote (("^epsf$" "sumatraPDF.exe %f") ("." "yap -1 %dS %d"))))
;; (setq Tex-output-view-style
;;       (quote
;;        (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start %f")
;; 	("^dvi$" "." "yap -1 %dS %d")
;; 	("^pdf$" "." "SumatraPDF.exe -reuse-instance %o")
;; 	("^html?$" "." "start %o"))))


(setq TeX-auto-untabify t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(provide 'auctex-config)