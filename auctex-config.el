;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AucTeX mode
;; Commented for faster load-times

;; (load "auctex.el" t)
;; (load "preview-latex.el" nil t t)
(when (eq system-type 'windows-nt)
  (require 'tex-mik))

;; Potential settings
; (setq TeX-view-style (quote (("^epsf$" "sumatraPDF.exe %f") ("." "yap -1 %dS %d"))))
;; (setq Tex-output-view-style
;;       (quote
;;        (("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "dvips %d -o && start %f")
;; 	("^dvi$" "." "yap -1 %dS %d")
;; 	("^pdf$" "." "SumatraPDF.exe -reuse-instance %o")
;; 	("^html?$" "." "start %o"))))

;; RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; Misc Settings
(setq TeX-auto-untabify t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

;; Extra Hooks
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'orgtbl-mode)

;; PDFLaTeX
(setq TeX-PDF-mode t)
(setq TeX-engine 'pdflatex)

;; Ask for master file
(setq-default TeX-master t)

(setq TeX-output-view-style
      (quote
       (("^pdf$" "." "evince -f %o")
        ("^html?$" "." "start %o"))))

(setq TeX-engine 'xetex)

;;("XeLaTeX" "%`xelatex %' %t" TeX-run-TeX nil (latex-mode doctex-mode) :help "Run XeLaTeX")


(provide 'auctex-config)
