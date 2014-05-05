;;; package --- Summary
;;; Commentary:
; providing the basic setup for org mode

(require 'org)
(setq org-log-done 'time)

;; adding more states to the TODO cycle
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "LIMBO")))

;; and adding different faces to them
;; based on solarized dark, may change
;; (setq org-todo-keyword-faces '(("TODO" . "#F2804F")
;; 			       ("IN-PROGRESS" . "#69B7F0")
;; 			       ("WAITING" . "#9EA0E5")
;; 			       ("DONE" . "#F771AC")
;; 			       ("LIMBO" . "#DEB542")))

;; org-agenda
(setq org-agenda-files (list "~/Desktop/non sottovalutare le conseguenze/delle letture.org"
			     "~/Desktop/non sottovalutare le conseguenze/dei progetti.org"
			     "~/Desktop/non sottovalutare le conseguenze/degli oneri.org"
			     "~/Desktop/non sottovalutare le conseguenze/della ricerca.org"))

;; customizing C-c a a
(setq org-agenda-custom-commands
      '(("a" "custom agenda"
         ((org-agenda-list nil nil 1)
          (tags "IN-PROGRESS")
          (tags-todo "WAITING")
          (tags-todo "DONE")
	  (tags-todo "LIMBO")))))

;; follow links by pressing RET
(setq org-return-follows-link t)

;; bindings
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)


;; customizing the bullets (correct vis depends on the font used)
;; (require 'org-bullets)
;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-bullets-bullet-list '("▹"
;;                                 "▪"
;;                                 "▫"
;; 				"•"
;; 				"・"
;; 				"✪"
;; 				"◎"))

(provide 'org-mode-setup)
;;; org-mode-setup ends here
