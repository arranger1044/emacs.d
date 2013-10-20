;;; package rano-packages.el Summary
;;; Commentary:
;;; begin rano-packages.el


;; package management

;;; Code


;; auto-indent
;(setq auto-indent-on-visit-file t) ;; If you want auto-indent on for files
(require 'auto-indent-mode)
(auto-indent-global-mode)

;; rainbox parenthesis (used for each programming language)
(require 'rainbow-delimiters)
;(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

;; frame-restore to restore last frame sizes and position
(add-to-list 'load-path "~/.emacs.d/rano/frame-restore")
(require 'frame-restore)

;; auto-complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)

(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;;(add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

;; for balancing parenthesys
(electric-pair-mode t)

;; for auto-indenting
(electric-indent-mode 1)

;; icicles for mini-buffer fast completion
					;(require 'icicles)

;; idomenu for mini-buffer real fast completion
(require 'ido)
;(ido-mode t) 
;(setq ido-enable-flex-matching t) 

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(autoload 'idomenu "idomenu" nil t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
;(setq flx-ido-use-faces nil)
					;(require 'idomenu)

(require 'smex) ; Not needed if you use package.el
(smex-initialize)

;;helm for substituting ido+smex?
					;(require 'helm-config)
					;(helm-mode 1)

;; fly-make for inline static syntax messages
;;(require 'flymake)
;;(require 'cmake-project)
;;(require 'flymake-cursor)
;;(flymake-mode t)

;; flycheck is supposed to be better than flymake
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; minor mode for coloring the mode line (in case of errors and warnings)
;; (defun flycheck-clang-include-local-dir ()
;;   "Add the current dir to the clang checker include list"
;;   (if (derived-mode-p 'c-mode 'c++-mode)
;;       (setq flycheck-clang-include-path (list (file-name-directory (buffer-file-name))))))
;; (add-hook 'flycheck-before-syntax-check-hook 'flycheck-clang-include-local-dir)

(defun flycheck-c++11-support-enabled ()
  "Enable the c++11 standard support for clang"
  (if (derived-mode-p 'c++-mode)
     (setq flycheck-clang-language-standard "c++11"
           flycheck-clang-standard-library  "libc++")))
(add-hook 'flycheck-before-syntax-check-hook 'flycheck-c++11-support-enabled)

;; clang-format.el is modified to include Allman's style inline atm
(load "~/.emacs.d/rano/clang-format/clang-format.el")
(global-set-key [C-M-tab] 'clang-format-region)
;; automatically format before saving in c++/c-modes
(defun clang-format-before-save ()
  (interactive)
  (when (or (eq major-mode 'c++-mode)
	    (eq major-mode 'c-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'clang-format-before-save)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; procjectile for project management
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)


;; magit for git integration
(require 'magit)

;; yasnippet for text snippet
(require 'yasnippet)
;;(setq yas/root-directory '("~/.emacs.d/rano/snippets"))
;; Map `yas/load-directory' to every element
;;(mapc 'yas/load-directory yas/root-directory)
(yas-global-mode 1)

;; workgroups2 for session management
;(require 'workgroups2)
;(setq wg-default-session-file "~/.emacs.d/rano/workgroups/.emacs_workgroups")
;(workgroups-mode 1)
;(require 'desktop)
;(desktop-restore-in-current-display t)
					;(require 'desktop)
;(setq desktop-restore-frames nil)
(desktop-save-mode 1)

;(require 'workgroups2)
;(setq wg-use-default-session-file nil
;      wg-default-session-file  "~/.emacs.d/rano/workgroups/session")
;(add-hook 'auto-save-hook 'wg-update-current-workgroup-working-wconfig)
;(add-hook 'emacs-startup-hook 'wg-reload-session)
;(workgroups-mode 1)


;; powerline
(require 'powerline)
(setq powerline-default-separator 'curve)
(powerline-reset)
(powerline-default-theme)

;; flycheck-color-mode-line
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; processing mode
(require 'processing-mode)
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/usr/bin/processing-java")
(setq processing-application-dir "/Applications/Processing.app")
(setq processing-sketchbook-dir "~/Documents/Processing")

;; prolog mode
;;(add-to-list 'load-path "~/.emacs.d/rano/prolog")
(require 'prolog)
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq prolog-program-name "/usr/local/bin/swipl")
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                              auto-mode-alist))

(provide 'rano-packages)
;;; rano-packages.el ends here
