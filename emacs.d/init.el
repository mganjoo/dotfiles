;;; init.el --- My init file.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; We won't use customizations, so save separately and ignore them.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Add custom lisp packages to the load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management: set up repositories.
(require 'package)
(setq package-enable-at-startup nil)
(eval-when-compile
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("org" . "https://orgmode.org/elpa/") t))
(package-initialize)

;; Install and configure use-package for other packages in file.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Basic configuration.
(setq
 inhibit-startup-screen t      ;; Disable startup screen
 initial-scratch-message ""    ;; Remove message from scratch buffer
 ring-bell-function 'ignore    ;; Disable the error bell
 column-number-mode t          ;; Display current column in addition to line
 make-backup-files nil         ;; Disable all backup file creation
 vc-follow-symlinks t          ;; Don't confirm when opening symlinked files
 use-package-always-ensure t)  ;; Avoid having to do ":ensure t" everywhere

;; Support for diminished minor modes (no modeline display).
(use-package diminish)

;; Appearance and theme.
(use-package leuven-theme
  :config
  (setq leuven-scale-outline-headlines nil)
  (load-theme 'leuven t))
(global-hl-line-mode 1)                  ;; Highlight current line
(set-face-background 'hl-line "#ffffd7") ;; ... with yellow
(add-to-list 'default-frame-alist        ;; Default editor font
             '(font . "Fira Mono-14"))

;; Make <ctrl+cmd+f> work on Mac for fullscreen.
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;; Some good defaults (https://github.com/technomancy/better-defaults).
;; TODO: can we directly configure some of these?
(use-package better-defaults)

;; Use PATH and other env variables from shell.
(use-package exec-path-from-shell
  :defer 1  ;; deferring seems to get rid of warning message about zhsrc
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Modeline.
(use-package powerline
  :config
  (use-package powerline-evil)
  (setq powerline-default-separator 'nil) ;; no patching, no separator
  (powerline-evil-center-color-theme))    ;; color-code evil state

;; Display possible key bindings for an incomplete command.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Vimify using evil.
(use-package evil
  :config
  ;; Load `evil-leader' and enable mode before `evil-mode'
  ;; so it's available in every initial buffer.
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ":"     'eval-expression
      "<SPC>" 'helm-M-x
      "e"     'flycheck-list-errors
      "d"     'deft
      "oa"    'org-agenda
      "oc"    'org-capture
      "ol"    'org-store-link))

  (evil-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  (use-package evil-commentary
    :diminish evil-commentary-mode
    :config
    (evil-commentary-mode))

  ;; Use emacs state in the following modes.
  (dolist (mode '(flycheck-error-list-mode
                  deft-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Helm.
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)

  ;; Use Helm-enhanced M-x.
  (global-set-key (kbd "M-x") 'helm-M-x)

  (use-package helm-descbinds
    :config
    (helm-descbinds-mode))

  ;; Flip <tab> and C-z mappings
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  (defun mg/remove-cursor-in-helm-buffers ()
    "Remove cursor in helm buffers."
    (with-helm-buffer (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook
            'mg/remove-cursor-in-helm-buffers))

;; Code completion.
(use-package company
  :diminish company-mode
  :config
  (global-company-mode)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;; Error checking.
(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

;; Relative line numbering.
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (setq nlinum-relative-redisplay-delay 0)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (add-hook 'org-mode-hook 'nlinum-relative-mode))

;; ENSIME
(use-package ensime)

;; Org
(use-package org
  :ensure org-plus-contrib
  :config
  (setq
   org-directory "~/Dropbox/org"
   org-default-notes-file (concat org-directory "/notes.org")
   org-todo-keywords
   '((sequence "TODO" "|" "DONE")
     (sequence "QUESTION" "|" "ANSWERED"))
   org-agenda-files '("~/Dropbox/org/" "~/Dropbox/notes/")
   org-log-done t)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)

  (use-package org-agenda
    :ensure nil
    :config
    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "j" 'org-agenda-next-item)
                (define-key org-agenda-mode-map "k" 'org-agenda-previous-item))))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (emacs-lisp . t)))
  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
  (use-package org-evil))

;; Deft
(use-package deft
  :commands (deft)
  :config
  (setq
   deft-extensions '("org" "md")
   deft-default-extension "org"
   deft-directory "~/Dropbox/notes"
   deft-use-filter-string-for-filename t
   deft-use-filename-as-title t)
  (add-hook 'deft-mode-hook
            (lambda ()
              (define-key deft-mode-map (kbd "C-c x")
                'kill-this-buffer))))

;; Column indicator at right margin.
(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

;; Allow folding of code blocks.
(use-package hideshow
  :diminish hs-minor-mode
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

;; Handy bracket mappings (local package).
(use-package evil-unimpaired
  :ensure nil)

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq
   projectile-enable-caching t
   projectile-completion-system 'helm))

(use-package magit
  :config
  (global-magit-file-mode)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (use-package company-anaconda
    :config
    (add-to-list 'company-backends
                 '(company-anaconda :with company-capf))))

(defun mg/prog-mode-electric-pair ()
  "Set up auto-closing of matching parentheses in `prog-mode'."
  (electric-pair-local-mode)) ;; Auto-closing matching parentheses
(add-hook 'prog-mode-hook 'mg/prog-mode-electric-pair)

(provide 'init)
;;; init.el ends here
