;;; package --- Summary

;;; Commentary:
;;; begin rano-bindings.el

;; key bindings for defaults and packages

;;; Code:
;; smex for M-x completion
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; sr-speedbar
(global-set-key (kbd "s-.") 'sr-speedbar-toggle)

;;; IDO

;; ido-file-find for fast file visiting
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; idomenu
(global-set-key (kbd "C-x C-i") 'idomenu)
;; 
;(global-set-key (kbd "M-i") 'ido-goto-symbol)

;; find-file in project
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)


;; auto-complete-clang
(global-set-key (kbd "C-'") 'ac-complete-clang)


;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)

;; highlight symbol
(global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "S-<f3>") 'highlight-symbol-prev)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-prev)

;; undo-tree remapping
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo) 

;;expand-region
(global-set-key (kbd "C-.") 'er/expand-region)

;; copying leaving the region highlighted
(defun kill-ring-save-keep-highlight (beg end)
  "Keep the region active after the kill."
  (interactive "r")
  (prog1 (kill-ring-save beg end)
    (setq deactivate-mark nil)))

(global-set-key (kbd "M-W") 'kill-ring-save-keep-highlight)

;; right click to forword search a symbol under the mouse
(defun xah-click-to-search (πclick)
  "Mouse click to start `isearch-forward-symbol-at-point' (Emacs 24.4) at clicked point."
  (interactive "e")
  (let ((p1 (posn-point (event-start πclick))))
    (goto-char p1)
    (isearch-forward-symbol-at-point)
    ))
(global-set-key (kbd "<mouse-3>") 'xah-click-to-search)

;; brackets and special chars under mac os x with an italian keyboard
;; (define-key global-map (kbd "M-è") "[")
;; (define-key global-map (kbd "M-+") "]")

;; (define-key global-map (kbd "M-é") "{")
;; (define-key global-map (kbd "M-*") "}")

;; (define-key global-map (kbd "M-à") "#")

;; (define-key global-map (kbd "M-ò") "@")

;; (define-key global-map (kbd "M-e") "€")

;; (define-key global-map (kbd "M-5") "~")

;; using command as meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'none)



(provide 'rano-bindings)
;;; rano-bindings.el ends here
