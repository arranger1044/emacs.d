;;; begin rano-bindings.el

;; key bindings for defaults and packages


;; smex for M-x completion
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; IDO

;; ido-file-find for fast file visiting
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;; idomenu
(global-set-key (kbd "C-x C-i") 'idomenu)

;; find-file in project
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)


;; auto-complete-clang
(global-set-key (kbd "C-'") 'ac-complete-clang)


;; undo-tree remapping
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo) 


;; icicles binding
					;(icy-mode 1)

;; brackets and special chars under mac os x with an italian keyboard
(define-key global-map (kbd "M-è") "[")
(define-key global-map (kbd "M-+") "]")

(define-key global-map (kbd "M-é") "{")
(define-key global-map (kbd "M-*") "}")

(define-key global-map (kbd "M-à") "#")

(define-key global-map (kbd "M-ò") "@")

(define-key global-map (kbd "M-e") "€")

(define-key global-map (kbd "M-5") "~")




(provide 'rano-bindings)
;;; end rano-bindings.el
