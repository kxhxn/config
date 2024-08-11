;; perf settings
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
  (setq use-package-always-defer t)
  (global-hl-line-mode t)
  (global-display-line-numbers-mode t)
  (electric-pair-mode t)
  (global-set-key (kbd "C-x C-b") 'ibuffer))

(use-package doom-themes
  :straight t)

(use-package theme-changer
  :straight t
  :demand t
  :init
  (setq calendar-latitude 5.55)
  (setq calendar-longitude -0.22)
  :config
  (change-theme 'doom-one-light 'doom-one))

(use-package which-key
  :straight t
  :demand t
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
  :config (add-to-list 'eglot-server-programs '(d-mode . ("serve-d"))))

(use-package company
  :straight t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package geiser-mit
  :straight t)

(use-package d-mode
  :straight t
  :hook (d-mode . eglot-ensure))

(use-package move-mode
  :straight t
  ;; :hook (move-mode . eglot-ensure)
  )

(use-package markdown-mode
  :ensure t
  :magic "\\.md\\'")
