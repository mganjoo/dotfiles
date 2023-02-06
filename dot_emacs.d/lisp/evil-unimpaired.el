;;; evil-unimpaired.el --- Pairs of handy bracket mappings.
;; -*- mode: emacs-lisp -*-

;;; Commentary:

;; Based on @tpope's unimpaired.vim
;; (https://github.com/tpope/vim-unimpaired).
;;
;; Adapted from
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+spacemacs/spacemacs-evil/local/evil-unimpaired/evil-unimpaired.el

;;; Code:

(require 'flycheck)
(require 'evil)

(defun mg/insert-newline-above (count)
  "Insert COUNT new lines above point and place point in the top line."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-above))))

(defun mg/insert-newline-below (count)
  "Insert COUNT new lines below point and leave point at same position."
  (interactive "p")
  (save-excursion (dotimes (_ count) (evil-insert-newline-below))))

(evil-global-set-key 'normal (kbd "[ b") 'previous-buffer)
(evil-global-set-key 'normal (kbd "] b") 'next-buffer)
(evil-global-set-key 'normal (kbd "[ SPC") 'mg/insert-newline-above)
(evil-global-set-key 'normal (kbd "] SPC") 'mg/insert-newline-below)

(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map
              (kbd "] l") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map
              (kbd "[ l") 'flycheck-previous-error)))

(provide 'evil-unimpaired)
;;; evil-unimpaired.el ends here
