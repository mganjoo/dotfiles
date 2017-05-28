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
;; Don't show warnings for redefined functions.
(setq ad-redefinition-action 'accept)

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
      "<SPC>" 'helm-M-x))

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
  :ensure t
  :diminish helm-mode
  :config
  (helm-mode 1)

  ;; Use tab for tab completion
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  ;; Use C-z to list actions
  (define-key helm-map (kbd "C-z")  'helm-select-action)

  ;; Bindings
  (global-set-key (kbd "M-x") 'helm-M-x)

  (use-package helm-descbinds
    :ensure t
    :config
    (helm-descbinds-mode))

  ;; Get rid of cursor in helm buffers.
  (add-hook 'helm-after-initialize-hook
            (lambda ()
              ;; To fix error at compile:
              ;; Forgot to expand macro with-helm-buffer in
              ;; (with-helm-buffer (setq cursor-in-non-selected-windows nil))
              (if (version< "26.0.50" emacs-version)
                  (eval-when-compile (require 'helm-lib)))
              (with-helm-buffer (setq cursor-in-non-selected-windows nil)))))

;; Theme.
(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))

;; Code completion.
(use-package company
  :ensure t
  :diminish company-mode
  :defer 1
  :config
  (global-company-mode)

  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previos))

;; Error checking.
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(provide 'init)
;;; init.el ends here
