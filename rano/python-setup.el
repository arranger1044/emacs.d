;;; package --- Summary
;;; Commentary:
; providing my customization for python-mode and related stuff

;;(require 'python)
(require 'python-mode)

;; (setq-default python-indent-guess-indent-offset nil)
;; (setq-default python-indent-offset 4)
;; (setq-default python-indent 4)

;(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

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

;; (add-hook 'python-mode-hook (lambda () (setq python-indent-offset 4)))

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

(setq py-smart-indentation t)


;; -----------------------------
;; emacs IPython notebook config
;; -----------------------------

                                        ; use autocompletion, but don't start to autocomplete after a dot
;; (setq ein:complete-on-dot -1)
;; (setq ein:use-auto-complete 1)

;; ;; ; set python console args
;; (setq ein:console-args
;;       (if (system-is-mac)
;;           '("--gui=osx" "--matplotlib=osx" "--colors=Linux" "--pylab")
;;         (if (system-is-linux)
;;             '("--gui=wx" "--matplotlib=wx" "--colors=Linux" "--pylab"))))

;; ;; ; timeout settings
;; (setq ein:query-timeout 1000)

;; ; IPython notebook
;; ;;(include-plugin "emacs-ipython-notebook/lisp")
;; (require 'ein)

;; ; shortcut function to load notebooklist
;; (defun load-ein ()
;;   (ein:notebooklist-load)
;;   (interactive)
;;   (ein:notebooklist-open))

; Set PYTHONPATH, because we don't load .bashrc
(defun set-python-path-from-shell-PYTHONPATH ()
  (let ((path-from-shell (shell-command-to-string "$SHELL -i -c 'echo $PYTHONPATH'")))
    (setenv "PYTHONPATH" path-from-shell)))
(if window-system (set-python-path-from-shell-PYTHONPATH))

;; autopep8 for autoformatting
(require 'py-autopep8)
(add-hook 'python-mode-hook '(lambda ()
			       (add-hook 'before-save-hook 'py-autopep8-before-save)))
;(add-hook 'before-save-hook 'py-autopep8-before-save)

;; keybinding for ipython interpreter
(add-hook 'python-mode-hook
          (lambda ()
	    (local-set-key (kbd "C-c i") 'py-shell)
            ))

(provide 'python-setup)
;;; python-setup ends here
