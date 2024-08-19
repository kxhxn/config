					; perf settings
;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

;; config
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package emacs
  :straight t
  :config
  (setq make-backup-files nil)
  ;; (setq use-package-always-defer t)
  (global-hl-line-mode t)
  (global-display-line-numbers-mode t)
  (electric-pair-mode t)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (delete-selection-mode t))

(use-package doom-themes
  :straight t)

(use-package theme-changer
  :straight t
  :init
  (setq calendar-latitude 5.55)
  (setq calendar-longitude -0.22)
  :config
  (change-theme 'doom-one-light 'doom-one))

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 0.25)
  (which-key-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-window
  :straight t
  :bind ("M-o" . ace-window))

(use-package dirvish
  :straight t
  :init (dirvish-override-dired-mode)
  :bind ("<f2>" . dirvish-side))

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package eglot
  :straight nil
  :config (add-to-list 'eglot-server-programs '(d-mode . ("serve-d")))
  (add-to-list 'eglot-server-programs '(move-mode "move-analyzer")))

(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package vertico
  :straight t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package format-all
  :straight t
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package smart-comment
  :straight t
  :bind ("M-;" . smart-comment))

(use-package exec-path-from-shell
  :straight t
  :init (exec-path-from-shell-initialize))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package ws-butler
;;   :straight t
;;   :hook (prog-mode . ws-butler-mode))

;; Languages
(use-package geiser-mit
  :straight t)

(use-package move-mode
  :straight t
  :hook (move-mode . eglot-ensure))

(use-package markdown-mode
  :straight t
  :magic "\\.md\\'")

(use-package csproj-mode
  :straight t)
(use-package sharper
  :straight t
  :bind
  ("C-c n" . sharper-main-transient)
  :config (setq sharper-run-only-one t))
;; (use-package dotnet
;;   :straight t
;;   :hook (csharp-mode . dotnet-mode)
;;   :bind-keymap
;;   ("C-c n" . dotnet-mode-command-map))
(use-package csharp-mode
  :straight nil
  :hook
  (csharp-mode . eglot-ensure))
