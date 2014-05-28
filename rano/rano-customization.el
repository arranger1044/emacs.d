;;; begin rano-customization.el

;; look&feel customizations

;;override theme (must come after package initialization
(setq solarized-high-contrast-mode-line t)
(setq solarized-distinct-fringe-background t)
(setq solarized-use-more-italic t)
(load-theme 'solarized-dark t)

;; (require 'color-theme-sanityinc-tomorrow)
;; (color-theme-sanityinc-tomorrow-eighties)

;; (require 'moe-theme)
;; ;; Choose the one you like, (moe-light) or (moe-dark)
;; (moe-light)
;; (setq moe-theme-mode-line-color 'orange)
;(powerline-moe-theme)

;;(load-theme 'solarized-light t)
;;(load-theme 'misterioso t)
;;(load-theme 'zenburn t)

;; (setq powerline-color1 "#073642")
;; (setq powerline-color2 "#002b36")

;; (set-face-attribute 'mode-line nil
;;                     :foreground "#fdf6e3"
;;                     :background "#2aa198"
;;                     :box nil)
;; (set-face-attribute 'mode-line-inactive nil
;;                     :box nil)

;; powerline
;; (require 'powerline)
;; ;; ;;(setq powerline-default-separator 'curve)
;; ;; ;; (powerline-reset)
;; ;; ;;(powerline-default-theme)
;; (powerline-center-theme)

;; flycheck-color-mode-line
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; custom-font
;(add-to-list 'default-frame-alist '(font . "Inconsolata 14"))
					;(set-frame-font "Inconsolata 13")
					;(set-face-attribute 'default nil :font "Inconsolata-13")
(cond ((system-is-mac) (set-face-attribute 'default nil :font "Inconsolata-13:bold"))
      ((system-is-linux) (set-face-attribute 'default nil :font "Inconsolata-10:bold")))


;; line numbers
(global-linum-mode t)

;; I like the toolbar even if it is ugly
					;(tool-bar-mode 1)
					; maybe not anymore
(tool-bar-mode -1)

;; I like the scroll bar too
(set-scroll-bar-mode 'right)

;; blinking cursor
(blink-cursor-mode t)

;; no statup messages
(setq inhibit-startup-message t)

;; shorten yes-no messages
(fset 'yes-or-no-p 'y-or-n-p)

;; putting all the auto-saved files togethe
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)

;; Disable abbrev save to prevent weird annoying
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq save-abbrevs nil)

;; Default encoding for Emacs in terminal
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; clipboard
(setq mac-pass-option-to-system t
      x-select-enable-clipboard t)

;; Make any instance of Emacs know my PATH well
(setenv "PATH" (shell-command-to-string "echo $PATH"))

;; writing on a selected region
(delete-selection-mode 1)

;; Forcing show-paren-mode
(require 'paren)
(setq show-paren-style 'parenthesis)
(show-paren-mode +1)

(require 'smartparens-config)
;; ;; (smartparens-mode t)
(show-smartparens-global-mode +1)
(smartparens-global-mode t)

					;(setq show-paren-style 'parentheses)
;(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
;(setq blink-matching-paren t)


;; visible bell instead of alarm
(setq visible-bell t)

;; Scroll line by line
(setq scroll-step 1)

;; Highlight line
(global-hl-line-mode 1)

;; Cursor color
(add-to-list 'default-frame-alist '(cursor-color . "goldenrod"))

;; c indentation style
(setq c-default-style "bsd"
      c-basic-offset 4)


;; highlight indentation
(set-face-background 'highlight-indentation-face "#073642")
(set-face-background 'highlight-indentation-current-column-face "#586e75")

;; adding a newline if going after the last lin
(setq next-line-add-newlines t)

;; srgb enabling under os x
(setq ns-use-srgb-colorspace t)

(provide 'rano-customization)
;;; rano-customization.el ends here
