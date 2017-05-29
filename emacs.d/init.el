;;; init.el --- My init file.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; Don't clutter init.el file with custom stuff.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Package management.
(require 'package)
(setq package-enable-at-startup nil)
(eval-when-compile
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

;; Some good defaults.
(use-package better-defaults)

;; Use PATH and other env variables from shell.
(use-package exec-path-from-shell
  :defer 1
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Font.
(add-to-list 'default-frame-alist '(font . "Fira Mono-14"))

;; Theme.
(use-package leuven-theme
  :config
  (load-theme 'leuven t))

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
      "e"     'flycheck-list-errors))

  (evil-mode 1)

  (use-package evil-surround
    :config
    (global-evil-surround-mode))

  (use-package evil-visualstar
    :config
    (global-evil-visualstar-mode))

  (use-package evil-commentary
    :config
    (evil-commentary-mode))

  ;; Use emacs state in the following modes.
  (dolist (mode '(flycheck-error-list-mode))
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

  (defun mg--remove-cursor-in-helm-buffers ()
    "Remove cursor in helm buffers."
    (with-helm-buffer (setq cursor-in-non-selected-windows nil)))
  (add-hook 'helm-after-initialize-hook 'mg--remove-cursor-in-helm-buffers))

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
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; Some good defaults for prog-mode.
(defun mg--set-up-prog-mode ()
  "Configure global `prog-mode' with basic defaults."
  (setq-local comment-auto-fill-only-comments t)
  (electric-pair-local-mode))
(add-hook 'prog-mode-hook 'mg--set-up-prog-mode)

(provide 'init)
;;; init.el ends here
