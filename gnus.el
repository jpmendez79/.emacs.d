;;; Gnus --- gnus config
;;; Commentary:
;;;Elcamino and Cox using MSMTP for multiple servers
;;; Code:

;; Needs to be moved to an appropriate section
;; (setq my-email-addresses '("jesse_mendez@elcamino.edu"
;;  			   "jessepmendez79@gmail.com"))
;; (setq message-alternative-emails
;;       (regexp-opt my-email-addresses))




;; ;; Gnus from manipulation
;; (setq gnus-from-selected-index 0)
;; (defun gnus-loop-from ()
;;   (interactive)
;;   (setq gnus-article-current-point (point))
;;   (goto-char (point-min))
;;   (if (eq gnus-from-selected-index (length my-email-addresses))
;;       (setq gnus-from-selected-index 0) nil)
;;   (while (re-search-forward "^From:.*$" nil t)
;;     (replace-match (concat "From: " user-full-name " <" (nth gnus-from-selected-index my-email-addresses) ">")))
;;   (goto-char gnus-article-current-point)
;;   (setq gnus-from-selected-index (+ gnus-from-selected-index 1)))

;; (global-set-key (kbd "C-c f") 'gnus-loop-from)

;; Adding code to access remote Imap servers for school and personal
(setq gnus-select-method '(nnmaildir "elcamino"
				     (directory "~/.mail/elcamino/")))
				     
(add-to-list 'gnus-secondary-select-methods '(nnmaildir "gmail"
						     (directory "~/.mail/gmail/")))

(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))

;; Configuring outgoing smtp servers
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq send-mail-function 'message-sendmail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
;; This is needed to allow msmtp to do its magic:
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))
(setq gnus-permanently-visible-groups ".*")
(setq gnus-thread-sort-functions
           '(gnus-thread-sort-by-most-recent-date))

;; Might be able to do this with eww
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)

(require 'gnus-desktop-notify)
(setq gnus-desktop-notify-function 'gnus-desktop-notify-exec
      gnus-desktop-notify-exec-program "notify-send -a Emacs")
(gnus-desktop-notify-mode)
;(gnus-demon-add-scanmail)

(provide '.gnus)
;;; .gnus ends here
