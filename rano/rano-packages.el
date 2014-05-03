;;; package rano-packages.el Summary
;;; Commentary:
;;; begin rano-packages.el


;; package management

;;; Code:


;;pakages
(when (>= emacs-major-version 24)
  (require 'package)
  
  (add-to-list 'package-archives '("melpa" .
				   "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" .
				   "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  )

;; auto installing when missing
(setq rano-packages
      '(
        auto-indent-mode
        rainbow-delimiters
	rainbow-mode
        frame-restore
        auto-complete
        auto-complete-config
        auto-complete-clang
	ace-jump-mode
        flx-ido
        smex
        flycheck
        undo-tree
        projectile
        magit
        powerline
        flycheck-color-mode-line
        processing-mode
        prolog
	sr-speedbar
	hideshowvis
        yasnippet
	fold-dwim
	smooth-scrolling
	auctex
	column-marker
	highlight-symbol
	cider
	smartparens
	ac-nrepl
	expand-region
	tabbar
	tabbar-ruler
	highlight-indentation
	python-mode
	jedi
	python-environment
	fic-mode
        ))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg rano-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))

(defun package-list-unaccounted-packages ()
  "Like `package-list-packages', but shows only the packages that
  are installed and are not in `rano-packages'.  Useful for
  cleaning out unwanted packages."
  (interactive)
  (package-show-package-list
   (remove-if-not (lambda (x) (and (not (memq x rano-packages))
                                   (not (package-built-in-p x))
                                   (package-installed-p x)))
                  (mapcar 'car package-archive-contents))))


;; sr-sppedbar
;; show the speedbar in the same frame (another window)
(require 'sr-speedbar)

;; enable hideshow folding widgets
(require 'hideshowvis)
(autoload 'hideshowvis-enable "hideshowvis" "Highlight foldable regions")

(autoload 'hideshowvis-minor-mode
  "hideshowvis"
  "Will indicate regions foldable with hideshow in the fringe."
  'interactive)

(dolist (hook (list 'emacs-lisp-mode-hook
                    'c++-mode-hook))
  (add-hook hook 'hideshowvis-enable))

(hideshowvis-symbols)
;(setq hs-set-up-overlay 'hs-abstract-overlay)

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
  (setq-default ac-sources '(ac-source-abbrev
			     ac-source-dictionary
			     ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;;(add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'cider-nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

;; for balancing parenthesys
;;(electric-pair-mode t)


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

;; smooth-scroll
;; (require 'smooth-scroll)
;; (smooth-scroll-mode t)

;; procjectile for project management
(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)


;; magit for git integration
(require 'magit)

;; yasnippet for text snippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/rano/snippets"))
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
;;(setq powerline-default-separator 'curve)
;; (powerline-reset)
;;(powerline-default-theme)
(powerline-center-theme)

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
(defun processing-mode-init ()
  (make-local-variable 'ac-sources)
  (setq ac-sources '(ac-source-dictionary ac-source-yasnippet))
  (make-local-variable 'ac-user-dictionary)
  (setq ac-user-dictionary (append processing-functions
                                   processing-builtins
                                   processing-constants)))

(add-to-list 'ac-modes 'processing-mode)
(add-hook 'processing-mode-hook 'processing-mode-init)


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

;; ace-jump mode
;; jumping throught text via char finding
(require 'ace-jump-mode)

;; (require 'sublimity)
;; ;; (require 'sublimity-scroll)
;; (require 'sublimity-map)
;; (setq sublimity-map-on-scroll t)
;; (sublimity-global-mode)
;; (sublimity-)
;; ;; (setq sublimity-scroll-weight 1
;; ;;       sublimity-scroll-drift-length 10)

(require 'smooth-scrolling)

;;(require 'minimap)

;; column-marker
; for highliting past 80 chars a column
(require 'column-marker)
(mapc (lambda (hook)
        (add-hook hook (lambda () (interactive) (column-marker-1 80))))
      '(c-mode-hook
        emacs-lisp-mode-hook
        c++-mode-hook
        text-mode-hook))

(require 'highlight-symbol)

;; auctex
(require 'tex-site)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)
(setq LaTeX-enable-toolbar t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)


;; setting the environment
;; (getenv "PATH")
(setenv "PATH"
        (concat
         "/usr/texbin" ":"
         "/usr/local/bin" ":"
         (getenv "PATH")))

(setq exec-path (append exec-path '("/usr/local/bin")))

;; (setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
;; (defun pdf-preview ()
;;   (add-to-list 'TeX-output-view-style
;;                '("^pdf$" "." "open -a Preview.app %s.pdf")))
;; (add-hook  'LaTeX-mode-hook  'pdf-preview)

(setq TeX-view-program-selection '((output-pdf "Preview")))
(setq TeX-view-program-list
      '(("Preview" "open -a Preview.app %s.pdf")))
(setq-default ispell-program-name "aspell")
(setq-default ispell-extra-args '("--reverse"))
(setq ispell-dictionary "italiano")
;; (setq TeX-view-program-list
;;       '(("Preview" "open -a Preview.app %s.pdf")))

;; ;; paredit
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code." t)

(require 'smartparens-config)
;; (smartparens-mode t)
(show-smartparens-global-mode +1)
(smartparens-global-mode t)

;; org mode setup
(require 'org-mode-setup)

;; winner mode
;; (when (fboundp 'winner-mode
;;(winner-mode 1))
(winner-mode 1)


;; expand-region
(require 'expand-region)


;;cider
(require 'cider)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)


;; tabbar-ruler
;; (setq tabbar-ruler-popup-toolbar t) ;
;; (setq tabbar-ruler-popup-scrollbar t)
;; (require 'tabbar-ruler)
;; (tabbar-ruler-group-by-projectile-project)
;; (global-set-key (kbd "C-c t") 'tabbar-ruler-move)


;;;;;;;;;;;;; python

;; highlight indentation
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook (lambda ()
			      (highlight-indentation-set-offset 4)))


;; jedi
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)


;------------- taken from
;------------- https://github.com/jhamrick/emacs



;; from python.el
;;(require 'python)
(require 'python-mode)

;; (setq-default python-indent-guess-indent-offset nil)
;; (setq-default python-indent-offset 4)
;; (setq-default python-indent 4)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(defun system-is-mac ()
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  (interactive)
  (string-equal system-type "gnu/linux"))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args (if (system-is-mac)
                                   "--gui=osx --matplotlib=osx --colors=Linux --pylab"
                                 (if (system-is-linux)
                                     "--gui=wx --matplotlib=wx --colors=Linux --pylab"))
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out \\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; fic-mode hihglighting FIXME TODO BUG
(require 'fic-mode)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)
(add-hook 'java-mode-hook 'turn-on-fic-mode)
(add-hook 'processing-mode-hook 'turn-on-fic-mode)
(add-hook 'python-mode-hook 'turn-on-fic-mode)

;; (add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))


;; -----------------------------
;; emacs IPython notebook config
;; -----------------------------

					; use autocompletion, but don't start to autocomplete after a dot
(setq ein:complete-on-dot -1)
(setq ein:use-auto-complete 1)

;; ; set python console args
(setq ein:console-args
      (if (system-is-mac)
          '("--gui=osx" "--matplotlib=osx" "--colors=Linux" "--pylab")
        (if (system-is-linux)
            '("--gui=wx" "--matplotlib=wx" "--colors=Linux" "--pylab"))))

;; ; timeout settings
(setq ein:query-timeout 1000)

; IPython notebook
;;(include-plugin "emacs-ipython-notebook/lisp")
(require 'ein)

; shortcut function to load notebooklist
(defun load-ein ()
  (ein:notebooklist-load)
  (interactive)
  (ein:notebooklist-open))

                                        ; Set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))

(if window-system (set-python-path-from-shell-PYTHONPATH))



(provide 'rano-packages)
;;; rano-packages.el ends here
