;; Elpaca init
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; End of elpaca init

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package emacs
  :ensure nil
  :config
  (setq make-backup-files nil)
  (setq use-package-always-defer t)
  (global-hl-line-mode t)
  (global-display-line-numbers-mode t)
  (electric-pair-mode t)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq warning-minimum-level :error)
  (delete-selection-mode 1)
  (xterm-mouse-mode t))

(use-package jsonrpc
  :ensure t)

(use-package xclip
  :ensure t
  :config (xclip-mode t))

(use-package company
  :ensure t
  :init
  (add-hook 'elpaca-after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.10)
  (setq company-minimum-prefix-length 2))
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package gcmh
  :ensure t
  :config (gcmh-mode t))

(use-package doom-themes
  :ensure t)

(use-package theme-changer
  :ensure t
  :demand t
  :init
  (setq calendar-latitude 5.55)
  (setq calendar-longitude -0.22)
  :config
  (change-theme 'doom-one-light 'doom-one))

(use-package which-key
  :ensure t
  :demand t
  :config
  (setq which-key-idle-delay 0.15)
  (which-key-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package dirvish
  :ensure t
  :init (dirvish-override-dired-mode)
  :bind ("<f2>" . dirvish-side))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-sync-connect nil)
  (add-to-list 'eglot-server-programs '(move-mode "move-analyzer")))
(use-package eglot-booster
  :ensure (:host github
		 :repo "jdtsmith/eglot-booster")
  :after eglot
  :init (eglot-booster-mode))		;; if :config then :demand t needs to be set

(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package format-all
  :ensure t
  :hook
  (prog-mode . format-all-mode)
  (format-all-mode . format-all-ensure-formatter))

(use-package smart-comment
  :ensure t
  :bind ("M-;" . smart-comment))

(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package dape
  :ensure t
  ;; :demand t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  (setq dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  ((kill-emacs . dape-breakpoint-save)
   ;; Load breakpoints on startup
   (elpaca-after-init . dape-breakpoint-load))

  :init
  ;; To use window configuration like gud (gdb-mi)
  (setq dape-buffer-window-arrangement 'gud)

  :config
  ;; Info buffers to the right
  ;; (setq dape-buffer-window-arrangement 'right)

  ;; Global bindings for setting breakpoints with mouse
  (dape-breakpoint-global-mode)

  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; To not display info and/or buffers on startup
  ;; (remove-hook 'dape-start-hook 'dape-info)
  ;; (remove-hook 'dape-start-hook 'dape-repl)

  ;; To display info and/or repl buffers on stopped
  ;; (add-hook 'dape-stopped-hook 'dape-info)
  ;; (add-hook 'dape-stopped-hook 'dape-repl)

  ;; Kill compile buffer on build success
  ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Save buffers on startup, useful for interpreted languages
  ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Projectile users
  (setq dape-cwd-fn 'projectile-project-root))

(use-package hl-todo
  :ensure t
  :config (global-hl-todo-mode))


;; Languages
(use-package geiser-mit
  :ensure t)

(use-package move-mode
  :ensure t
  :hook (move-mode . eglot-ensure))

(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")

(use-package csproj-mode
  :ensure t)
(use-package sharper
  :ensure t
  :bind ("C-c n" . sharper-main-transient)
  :config (setq sharper-run-only-one t))

(use-package csharp-mode
  :hook
  (csharp-mode . eglot-ensure)
  (csharp-ts-mode . eglot-ensure))
