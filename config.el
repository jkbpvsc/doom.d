;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "jjjj"
      user-mail-address "pseudorandomoracle@proton.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-xcode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq display-line-numbers 'relative)

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(modify-syntax-entry ?_ "w")

(image-type-available-p 'svg)

(use-package flycheck :ensure)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-lens--disable t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc--inline t)
  (lsp-inlay-hint-enable t)
  )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom

  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.1)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-cargo-watch-command "check")
  ;; (lsp-rust-analyzer-cargo-target "x86_64-apple-darwin")
  (lsp-rust-all-features t)

  (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  ;; (lsp-rust-analyzer-display-reborrow-hints t)

  ;; (setq gc-cons-threshold (* 1024 1024))
  (setq gcmh-high-cons-threshold 16777216)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb

  (global-set-key (kbd "s-d") 'lsp-describe-thing-at-point)

  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! (evil copilot)
  ;; Define the custom function that either accepts the completion or does the default behavior
  (defun my/copilot-tab-or-default ()
    (interactive)
    (if (and (bound-and-true-p copilot-mode)
             ;; Add any other conditions to check for active copilot suggestions if necessary
             )
        (copilot-accept-completion)
      (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.

  (setq copilot-log-max 1000)

  ;; Bind the custom function to <tab> in Evil's insert state
  (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; if you use typescript-mode
(add-hook 'typescript-mode-hook #'setup-tide-mode)
;; if you use treesitter based typescript-ts-mode (emacs 29+)
(add-hook 'typescript-ts-mode-hook #'setup-tide-mode)

(setq max-lisp-eval-depth 10000)

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(setq word-wrap t)

;; Typescript LSP Support

;; Install required packages if not already installed
(unless (package-installed-p 'typescript-mode)
  (package-refresh-contents)
  (package-install 'typescript-mode))

;; Enable TypeScript mode
(require 'typescript-mode)

;; Enable LSP mode for TypeScript
(add-hook 'typescript-mode-hook #'lsp-deferred)

;; Configure LSP for TypeScript
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-enabled-clients 'ts-ls)
  (add-to-list 'lsp-enabled-clients 'rust-analyzer)
  )

;; Use TypeScript language server
(setq lsp-clients-typescript-server "typescript-language-server")

;; Set up project-specific configurations
(defun my/typescript-project-setup ()
  "Set up LSP for TypeScript project."
  (when (and buffer-file-name
             (string-match-p "/my-typescript-project/" buffer-file-name))
    (lsp)))

;; Add project-specific setup hook
(add-hook 'typescript-mode-hook #'my/typescript-project-setup)

(use-package lsp-mode
  :hook
  ((python-mode . lsp)))

(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "pylsp")
                  :major-modes '(python-mode)
                  :server-id 'pylsp))

(with-eval-after-load "lsp-mode"
  (add-to-list 'lsp-disabled-clients 'pyls)
  (add-to-list 'lsp-enabled-clients 'pylsp ))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 2)

(add-hook 'typescript-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)))

(add-hook 'typescript-mode-hook 'prettier-rc-mode)
(add-hook 'js2-mode-hook 'prettier-rc-mode)
(add-hook 'web-mode-hook 'prettier-rc-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company lsp-ui lsp-mode typescript-mode vterm docker-tramp uwu-theme spacemacs-theme)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set Fantasque Sans Mono as the default font at size 12
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 105)

;; Enable anti-aliasing for better font rendering
(setq-default frame-resize-pixelwise t)
(setq-default face-font-rescale-alist '(("Hack Nerd Font" . 1.0)))

;; Optional: Configure line spacing to your preference
(setq-default line-spacing 0.1)

;; Optional: Set the default window frame size
(setq default-frame-alist
      '((width . 80)   ; Set the desired width
        (height . 40))) ; Set the desired height

(use-package! chatgpt
  :defer t
  :bind ("C-c q" . chatgpt-query))
