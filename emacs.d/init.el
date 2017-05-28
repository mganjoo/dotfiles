;; Don't litter the init file.
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

(use-package better-defaults :ensure t)

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
      ":" 'eval-expression))

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
    (evil-commentary-mode)))

(use-package leuven-theme
  :ensure t
  :config
  (load-theme 'leuven t))
