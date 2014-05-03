;;; package --- Summary
;;; Commentary:
; providing the basic setup for org mode

(require 'org)
(setq org-log-done 'time)

;; adding more states to the TODO cycle
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "LIMBO")))

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

(provide 'org-mode-setup)
;;; org-mode-setup ends here
