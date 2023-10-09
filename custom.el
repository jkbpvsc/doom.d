(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(prettier-rc company lsp-ui lsp-mode typescript-mode vterm docker-tramp uwu-theme spacemacs-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Set Fantasque Sans Mono as the default font at size 12
(set-face-attribute 'default nil
                    :family "Fantasque Sans Mono"
                    :height 120)

;; Enable anti-aliasing for better font rendering
(setq-default frame-resize-pixelwise t)
(setq-default face-font-rescale-alist '(("Fantasque Sans Mono" . 1.0)))

;; Optional: Configure line spacing to your preference
(setq-default line-spacing 0.1)

;; Optional: Set the default window frame size
(setq default-frame-alist
      '((width . 80)   ; Set the desired width
        (height . 40))) ; Set the desired height
