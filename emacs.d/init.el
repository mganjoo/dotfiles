;;; init.el --- My init file.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Boostrap package management.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'diminish)

;; Basic configuration.
(use-package better-defaults :ensure t)
(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)

;; Vimify.
(use-package evil
  :ensure t
  :config

  ;; Load `evil-leader' and enable mode before `evil-mode'
  ;; so it's available in every initial buffer.
  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>")
    (evil-leader/set-key
      ":"     'eval-expression
      "<SPC>" 'execute-extended-command))

  (evil-mode 1)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-visualstar
    :ensure t
    :config
    (global-evil-visualstar-mode))

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

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
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)
  (define-key evil-ex-map "e" 'helm-find-files))

;; Theme.
(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

;; Code completion.
(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode))

;; Error checking.
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(provide 'init)
;;; init.el ends here
