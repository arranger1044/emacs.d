(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (jedi zenburn-theme yaml-mode vagrant-tramp vagrant undo-tree tabbar-ruler sr-speedbar solarized-theme smooth-scrolling smex smartparens rainbow-mode rainbow-delimiters python-mode py-autopep8 projectile processing-mode moe-theme markdown-mode magit idomenu highlight-symbol hideshowvis git-timemachine frame-restore fold-dwim flycheck-color-mode-line flx-ido fic-mode expand-region ess elpy ein cython-mode color-theme-sanityinc-tomorrow clips-mode auto-complete-clang auctex ace-jump-mode ac-nrepl))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; main custom dir
(add-to-list 'load-path "~/.emacs.d/rano")

;; starting to add user  modules
(require 'rano-packages)
(require 'rano-customization)
(require 'rano-bindings)
;(workgroups-mode 1)



;;; init.el ends here
