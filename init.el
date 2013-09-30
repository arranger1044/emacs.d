(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
					;'(desktop-save-mode nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;pakages
(when (>= emacs-major-version 24)
  (require 'package)
  
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize)
  )


;; main custom dir
(add-to-list 'load-path "~/.emacs.d/rano")

;; starting to add user  modules
(require 'rano-packages)
(require 'rano-customization)
(require 'rano-bindings)
;(workgroups-mode 1)


;;; init.el ends here
