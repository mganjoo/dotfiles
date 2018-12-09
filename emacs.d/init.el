;;; init.el --- My init file.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;;; Code:

;; We won't use customizations, so save separately and ignore them.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Package management
;; ==================

(require 'package)
(setq package-enable-at-startup nil)
(eval-when-compile
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)

;; Install and configure `use-package' for other packages in file.
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
 make-backup-files nil         ;; Don't make backups when first saving files
 vc-follow-symlinks t          ;; Don't confirm when opening symlinked files
 use-package-always-ensure t   ;; Avoid having to do ":ensure t" everywhere
 require-final-newline t)      ;; Automatically add newlines at end of file

;; Support for diminished minor modes (no modeline display).
(use-package diminish)

;; Appearance
;; ==========

;; Theme
(use-package leuven-theme
  :config
  (setq leuven-scale-outline-headlines nil)
  (load-theme 'leuven t))
(global-hl-line-mode 1)                  ;; Highlight current line
(set-face-background 'hl-line "#ffffd7") ;; ... with yellow
(add-to-list 'default-frame-alist        ;; Default editor font
             '(font . "Fira Mono-14"))

;; Mode line
(use-package telephone-line
  :config
  (setq
   telephone-line-lhs '((evil   . (telephone-line-evil-tag-segment))
                        (accent . (telephone-line-process-segment))
                        (nil    . (telephone-line-minor-mode-segment
                                   telephone-line-buffer-segment)))
   telephone-line-rhs '((nil    . (telephone-line-misc-info-segment))
                        (accent . (telephone-line-major-mode-segment))
                        (evil   . (telephone-line-position-segment)))
   telephone-line-primary-left-separator    'telephone-line-flat
   telephone-line-secondary-left-separator  'telephone-line-nil
   telephone-line-primary-right-separator   'telephone-line-flat
   telephone-line-secondary-right-separator 'telephone-line-nil
   telephone-line-evil-use-short-tag        t)
  (telephone-line-mode 1))

;; Disable all visual artifacts.
(unless window-system
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Remove annoying startup echo area message.
(defun display-startup-echo-area-message () (message ""))

;; Configure mouse support (from https://www.iterm2.com/faq.html)
(unless window-system
 (require 'mouse)
 (xterm-mouse-mode t)
 ;; Enable natural scrolling (wheel up -> scroll down)
 (global-set-key [mouse-4] (lambda () (interactive) (scroll-down 3)))
 (global-set-key [mouse-5] (lambda () (interactive) (scroll-up 3))))

;; Make <ctrl+cmd+f> work on Mac for fullscreen.
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;; Use PATH and other env variables from shell in GUI environments.
(use-package exec-path-from-shell
  :config
   ;; Don't load interactive shell files
  (setq exec-path-from-shell-arguments '("-l"))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Display possible key bindings for an incomplete command.
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;; Vim-ify using evil.
(use-package evil
  :config
  ;; Load `evil-leader' and enable mode before `evil-mode'
  ;; so it's available in every initial buffer.
  (use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader "<SPC>"))

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
    (evil-commentary-mode)))

;; Column indicator at right margin.
(use-package fill-column-indicator
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

;; Diminish some built-in minor modes.
(use-package eldoc :diminish eldoc-mode)
(use-package undo-tree :diminish undo-tree-mode)

;; Auto-close matching parentheses in `prog-mode'.
(add-hook 'prog-mode-hook (lambda () (electric-pair-local-mode)))

;; Relative line numbers.
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode))

;; ivy mode
(use-package counsel ;; brings ivy and swiper as deps
  :diminish ivy-mode
  :diminish counsel-mode
  :config
  (ivy-mode 1)       ;; use ivy for `completing-read-function'.
  (counsel-mode 1)   ;; replace some built-in commands
  (setq
   ivy-use-virtual-buffers t)) ;; switch recent files and bookmarks also

;; Project management.
(use-package projectile
  :config
  (setq
   projectile-completion-system 'ivy
   projectile-enable-caching t      ;; cache file lists
   projectile-mode-line-function '(lambda ()
                                    (format "[%s]"
                                            (projectile-project-name))))
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (projectile-mode))


(setq-default
 indent-tabs-mode nil) ;; Never use tabs for indentation

(evil-global-set-key 'normal (kbd "[ b") 'previous-buffer)
(evil-global-set-key 'normal (kbd "] b") 'next-buffer)

(defun mganjoo/insert-newline-above (count)
  "Insert COUNT new lines above point and place point in the top line."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-above))))
(evil-global-set-key 'normal (kbd "[ SPC") 'mganjoo/insert-newline-above)

(defun mganjoo/insert-newline-below (count)
  "Insert COUNT new lines below point and leave point at same position."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-below))))
(evil-global-set-key 'normal (kbd "] SPC") 'mganjoo/insert-newline-below)

(provide 'init)
;;; init.el ends here
