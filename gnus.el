;;; Gnus --- gnus config
;;; Commentary:
;;;Elcamino and Cox using MSMTP for multiple servers
;;; Code:

;; Adding code to access remote Imap servers for school and personal
(setq gnus-select-method '(nnmaildir "elcamino"
				     (directory "~/.mail/elcamino/")))
				     
(add-to-list 'gnus-secondary-select-methods '(nnmaildir "gmail"
						     (directory "~/.mail/gmail/")))
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
(setq message-sendmail-envelope-from "header")

;; Might be able to do this with eww
;; (setq mm-text-html-renderer 'shr)
;; (setq mm-inline-text-html-with-images t)
;; (setq mm-inline-text-html-with-w3m-keymap nil)
;; (setq mail-user-agent 'message-user-agent)
;; (setq message-send-mail-function 'message-send-mail-with-sendmail)

(defun gnus-loop-from ()
  (interactive)
  (setq gnus-article-current-point (point))
  (goto-char (point-min))
  (if (eq gnus-from-selected-index (length my-email-addresses))
  (setq gnus-from-selected-index 0) nil)
   (while (re-search-forward "^From:.*$" nil t)
     (replace-match (concat "From: " user-full-name " <" (nth gnus-from-selected-index my-email-addresses) ">")))
   (goto-char gnus-article-current-point)
   (setq gnus-from-selected-index (+ gnus-from-selected-index 1)))

   (global-set-key (kbd "C-c f") 'gnus-loop-from)
(setq gnus-posting-styles
      '(((header "to" "jessepmendez79@gmail.com")
         (address "jessepmendez79@gmail.com"))
	(header "to" "jesse_mendez@elcamino.edu")
	(address "jesse_mendez@elcamino.edu")))

(provide '.gnus)
;;; .gnus ends here
