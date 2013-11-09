;;; begin rano-customization.el

;; look&feel customizations

;;override theme (must come after package initialization
(load-theme 'solarized-dark t)

;; custom-font
;(add-to-list 'default-frame-alist '(font . "Inconsolata 14"))
(set-frame-font "Inconsolata 14")

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


;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
(defconst emacs-tmp-dir (format "%s/%s%s/" temporary-file-directory "emacs" (user-uid)))
(setq backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix  emacs-tmp-dir)

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
(show-paren-mode t)
(setq show-paren-style 'parentheses)

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

(provide 'rano-customization)
;;; end rano-customization.el
