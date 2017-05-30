;;; init.el --- My init file.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; Don't clutter init.el file with custom stuff.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Add additional lisp packages to the load path.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Package management.
(require 'package)
(setq package-enable-at-startup nil)
(eval-when-compile
  (add-to-list 'package-archives
               '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(require 'diminish)

;; Basic configuration.
(setq
 inhibit-startup-screen t
 initial-scratch-message ""
 visible-bell nil
 ring-bell-function 'ignore
 column-number-mode t
 make-backup-files nil
 vc-follow-symlinks t
 ad-redefinition-action 'accept ;; No warnings for redefined functions
 use-package-always-ensure t)   ;; Avoid having to do ":ensure t" everywhere

;; Font.
(add-to-list 'default-frame-alist '(font . "Fira Mono-14"))

;; Some good defaults for prog-mode.
(defun mg/set-up-prog-mode ()
  "Configure global `prog-mode' with basic defaults."
  (setq-local comment-auto-fill-only-comments t)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'mg/set-up-prog-mode)

;; Some good defaults.
(use-package better-defaults)

;; Use PATH and other env variables from shell.
(use-package exec-path-from-shell
  :defer 1
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Theme.
(use-package leuven-theme
  :config
  (setq leuven-scale-outline-headlines nil)
  (load-theme 'leuven t)
  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#ffffd7"))

;; Modeline.
(use-package powerline
  :config
  (use-package powerline-evil)
  (setq powerline-default-separator 'nil)
  (powerline-evil-center-color-theme))

;; Vimify.
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

  (evil-global-set-key 'normal "]b" 'next-buffer)
  (evil-global-set-key 'normal "[b" 'previous-buffer)

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
    (add-to-list 'evil-emacs-state-modes mode))

  ;; Bindings for other packages.
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (evil-define-key 'normal flycheck-mode-map
                (kbd "]l") 'flycheck-next-error)
              (evil-define-key 'normal flycheck-mode-map
                (kbd "[l") 'flycheck-previous-error))))

;; Helm.
(use-package helm
  :diminish helm-mode
  :config
  (helm-mode 1)

  ;; Use enhanced M-x.
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

;; Finding out what a specific binding is for.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Code completion.
(use-package company
  :diminish company-mode
  :defer 1
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
(use-package ensime :pin melpa-stable)

;; Org
(use-package org
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
  (global-set-key (kbd "C-c c") 'org-capture))
(use-package org-agenda
  :config
  (define-key org-agenda-mode-map "j" 'org-agenda-next-item)
  (define-key org-agenda-mode-map "k" 'org-agenda-previous-item))
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(use-package org-evil)

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

;; Fill column indicator.
(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

;; Allow folding of code blocks.
(use-package hideshow
  :diminish hs-minor-mode
  :config
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; Undo tree.
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(provide 'init)
;;; init.el ends here
