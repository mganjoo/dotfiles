;; vim: ft=lisp
;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration layers declaration."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; Lazy install layers not in this file.
   dotspacemacs-enable-lazy-installation 'unused
   ;; Ask before lazy installation of a layer.
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
     '(
       auto-completion
       better-defaults
       emacs-lisp
       helm
       markdown
       org
       osx
       themes-megapack
       )
   ;; Additional, non-configured packages.
   dotspacemacs-additional-packages '()
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Install explictly used packages anre uninstall the rest.
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function. Called before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Use https for ELPA repos.
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; Do not check for updates at startup.
   dotspacemacs-check-for-update nil
   ;; Use `vim' editing style.
   dotspacemacs-editing-style 'vim
   ;; Do not show a banner.
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer.
   dotspacemacs-startup-lists '()
   ;; Make the home buffer respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Use `text-mode' for scratch buffer.
   dotspacemacs-scratch-mode 'text-mode
   ;; Themes to use.
   dotspacemacs-themes '(leuven)
   ;; Make the cursor color match state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font.
   dotspacemacs-default-font '("Fira Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key.
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key (equivalent of `<leader> m`).
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; Shift mappings `<' and `>' retain visual state if used there.
   dotspacemacs-retain-visual-state-on-shift t
   ;; Name of the default layout.
   dotspacemacs-default-layout-name "Default"
   ;; Threshold (in MB) for opening large files literally.
   dotspacemacs-large-file-size 1
   ;; Use cache directory for auto-saves.
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache.
   dotspacemacs-max-rollback-slots 5
   ;; Show `helm' at the bottom.
   dotspacemacs-helm-position 'bottom
   ;; Always use fuzzy matching in helm.
   dotspacemacs-helm-use-fuzzy 'always
   ;; Which-key delay in seconds.
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position.
   dotspacemacs-which-key-position 'bottom
   ;; Do not show a progress bar during load.
   dotspacemacs-loading-progress-bar nil
   ;; 90% transparency level of a frame when it's active or selected. (`toggle-transparency')
   dotspacemacs-active-transparency 90
   ;; 90% transparency level of a frame when it's inactive or dselected.
   dotspacemacs-inactive-transparency 90
   ;; Show the titles of transient states.
   dotspacemacs-show-transient-state-title t
   ;; Show the color guide hint for transient state keys.
   dotspacemacs-show-transient-state-color-guide t
   ;; Show unicode symbols in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; Use smooth scrolling.
   dotspacemacs-smooth-scrolling t
   ;; Use relative line numbers.
   dotspacemacs-line-numbers 'relative
   ;; Use `evil' code folding.
   dotspacemacs-folding-method 'evil
   ;; Highlight all delimiters.
   dotspacemacs-highlight-delimiters 'all
   ;; Search tools to use.
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;; Delete trailing whitespaces.
   dotspacemacs-whitespace-cleanup 'trailing
   ;; Default values for the following variables.
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-verbose-loading nil
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-display-default-layout nil
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-persistent-server nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code. Called after `dotspacemacs/init',
before layer configuration executes. Prefer setting things in `dotspacemacs/user-config'."
  (setq
   ;; Can't avoid having some environment variables in startup files.
   exec-path-from-shell-check-startup-files nil))

(defun dotspacemacs/user-config ()
  "Configuration function for user code. Called at the very end after layers configuration.
Prefer this place to set variables (unless it must be done before package load)."
  (setq
   ;; We use symlinks for managing dotfiles, so follow by default.
   vc-follow-symlinks t
   ;; No fancy powerline separators.
   powerline-default-separator 'nil))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl color-theme-sanityinc-tomorrow unfill mwim helm-company helm-c-yasnippet fuzzy flyspell-correct-helm flyspell-correct company-statistics company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete org-projectile org-present org-pomodoro alert log4e gntp org-download mmm-mode markdown-toc markdown-mode htmlize gnuplot gh-md ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zonokai-theme zenburn-theme zen-and-art-theme white-sand-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme rebecca-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme madhat2r-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-plus-contrib org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
