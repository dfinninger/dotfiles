;;; Startup Optimization =================================
;; Increases the Emacs GC threhold during startup--makes startup faster
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb


(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
;;; Editor Settings ======================================
;; Turn off menu bar, tool bar, and scroll bar.
(if (fboundp 'menu-bar-mode)   (menu-bar-mode   -1))
(if (fboundp 'tool-bar-mode)   (tool-bar-mode   -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off the splash screen.
(setq inhibit-startup-message t)

;; Don't use tabs for indentation.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Store backup files under ~/.emacs.d/backups
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

;; Show parenthesis when hovering over
(show-paren-mode 1)

;; Refresh from disk
(global-auto-revert-mode t)

;; Autosave
(auto-save-visited-mode t)

;; Show line numbers
(global-display-line-numbers-mode t)

;; Lockfiles remove lockfiles
(setq create-lockfiles nil)

;; fix the PATH variable
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (shell-command-to-string "TERM=vt100 $SHELL -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

;; Offload the custom-set-variables to a separate file. This keeps init.el neater.
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t) ; Load custom file. Don't hide errors. Hide success message


;; Disable lockfiles
(setq create-lockfiles nil)

;;; Packages ============================================
;; MELPA Stuff
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


;;; Configuration ========================================

;; Color Theme
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox t))

;; Evil Settings
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)

  (use-package evil-escape
    :ensure t
    :config (evil-escape-mode 1))

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode t)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key  ; leader-key definitions
      "TAB" 'evil-switch-to-windows-last-buffer
      "b b" 'ivy-switch-buffer
      "b d" 'kill-buffer
      "f f" 'find-file
      "f s" 'save-buffer
      "g g" 'magit-status
      "g d" 'magit-dispatch
      "g f" 'magit-file-dispatch
      "p" 'projectile-command-map
      "s s" 'swiper
      "t t" 'vterm-toggle-cd
      "d x w" 'delete-trailing-whitespace))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1))

  (use-package evil-org
    :ensure t
    :config
    (evil-org-set-key-theme
     '(textobjects insert navigation additional shift todo heading))
    (add-hook 'org-mode-hook (lambda () (evil-org-mode)))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Projectile
(use-package projectile
  :ensure t)

;; Ivy Settings
(use-package counsel
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")

  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)))

;; Magit
(use-package magit
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; Comany Mode
(use-package company
  :ensure t
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq comapny-idle-delay 0.3)
  (global-company-mode t))

;; vterm Shell REPL
(use-package vterm
  :ensure t
  :config
  (use-package vterm-toggle
    :ensure t))

;; Tree-sitter
(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)

;; Spaceline
(use-package spaceline
  :ensure t)

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-emacs-theme))

;; YAML
(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; LSP Mode =================================================
(use-package lsp-mode
  :ensure t
  :commands lsp)

(with-eval-after-load 'lsp-mode
  (evil-leader/set-key
    "l" lsp-command-map)
  (evil-normalize-keymaps))

(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
; not using treemacs now, but keeping it for later
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Python LSP
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))
