;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Personal Information
(setq user-full-name "jjjj"
      user-mail-address "pseudorandomoracle@proton.me")

;; Appearance & UI
(setq doom-theme 'doom-xcode
      display-line-numbers-type 'relative
      org-directory "~/org/")

;; Frame Settings: You can choose to maximize or specify a fixed size.
(setq default-frame-alist '((fullscreen . maximized)
                            (width . 80)
                            (height . 40)))

;; Font Configuration
(set-face-attribute 'default nil
                    :family "Hack Nerd Font"
                    :height 105)
(setq-default face-font-rescale-alist '(("Hack Nerd Font" . 1.0)))
(setq-default line-spacing 0.1)

;; Performance Tweaks
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;; Custom Window Splitting Functions with Follow Behavior
(defun split-and-follow-horizontally ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-and-follow-vertically ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))

(map! "C-x 2" #'split-and-follow-horizontally
      "C-x 3" #'split-and-follow-vertically)

;; DAP Mode (Debug Adapter Protocol)
(use-package! dap-mode
  :config
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

;; LSP Configuration
(use-package! lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :init
  (setq lsp-idle-delay 0.1
        lsp-eldoc-render-all t
        read-process-output-max (* 1024 1024))
  :config
  (lsp-enable-which-key-integration t))

(use-package! lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc--inline t))

;; Company Mode for Completion
(use-package! company
  :config
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.0
        company-minimum-prefix-length 1))

(use-package! company-box
  :hook (company-mode . company-box-mode))

;; GitHub Copilot Configuration
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word)))
(after! (evil copilot)
  (defun my/copilot-tab-or-default ()
    "Accept Copilot completion if available, otherwise insert a tab."
    (interactive)
    (if (bound-and-true-p copilot-mode)
        (copilot-accept-completion)
      (evil-insert 1)))
  (map! :map evil-insert-state-map "<tab>" #'my/copilot-tab-or-default))

;; TypeScript Configuration
(use-package! typescript-mode
  :hook ((typescript-mode typescript-ts-mode) . (lambda ()
                                                  (lsp-deferred)
                                                  (setq typescript-indent-level 2)
                                                  (prettier-rc-mode))))

;; Prettier: Enable for js2-mode and web-mode using Doom's add-hook!
(add-hook! '(js2-mode web-mode) #'prettier-rc-mode)

;; Python LSP Client Configuration Using pylsp
(use-package! python
  :hook (python-mode . lsp-deferred)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pylsp")
                    :major-modes '(python-mode)
                    :server-id 'pylsp))
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'pylsp)))

;; Optional ChatGPT Integration
(use-package! chatgpt
  :defer t
  :bind ("C-c q" . chatgpt-query))

;; Enable which-key for Easier Key Discovery
(which-key-mode)
