;;; Gnus --- gnus config
;;; Commentary:
;;;Elcamino and Cox using MSMTP for multiple servers
;;; Code:
;;(gnus-demon-add-handler 'gnus-demon-scan-news 2 t)
;;(gnus-demon-init)  
;; (setq gnus-select-method
;;       '(nnimap "Elcamino"
;; 	       (nnimap-address "127.0.0.1")
;; 	       (nnimap-server-port 1143)
;; 	       (nnimap-stream network)
;; 	       (nnimap-mailbox "INBOX")
;; 	       ;; (nnimap-split-rule
;; 	       ;; 	'(("INBOX.SI" "^From:Ngyuen, Connie")
;; 	       ;; 	  ("INBOX.SI" "^From:Barrueta, Luis")))
;; 	       )
;; 	       )

;; (setq gnus-select-method
;;       '(nnimap "Elcamino"
;; 	       (nnimap-address "localhost")
;; 	       (nnimap-authenticator login)
;; 	     ;  (nnimap-server-port 1143)
;; 	      ; (nnimap-mailbox "INBOX")
;; 	       (nnimap-stream network)))


(setq gnus-select-method '(nnimap "Elcamino"
                              (nnimap-stream shell)
                              (nnimap-shell-program "/usr/libexec/dovecot/imap -o mail_location=maildir:~/Mail:LAYOUT=fs")))

(add-to-list 'gnus-secondary-select-methods '(nnimap "gmail"
						     (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
						     (nnimap-server-port 993)
						     (nnimap-stream ssl)))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
;; (add-to-list 'gnus-secondary-select-methods '(nnimap "MendezCC"
;; 						     (nnimap-address "milo.mendezcc.com")  ; it could also be imap.googlemail.com if that's your server.
;; 						     (nnimap-server-port 993)
;; 						     (nnimap-stream ssl)))

(setq my-email-addresses '("jesse_mendez@elcamino.edu"
			   "jessepmendez79@gmail.com"))

(setq message-alternative-emails
      (regexp-opt my-email-addresses))


;; Gnus from manipulation
(setq gnus-from-selected-index 0)
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





(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq sendmail-program "/usr/bin/msmtp")
;(setq nnimap-split-inbox "INBOX")
;(setq nnimap-split-rule
;;       '(
;; 	("INBOX.Work" "^From:.*chnguyen@elcamino.edu")
;; 	("INBOX" " ")))
;; (setq nnimap-resplit-incoming t)
;; (setq nnimap-split-predicate "UNDELETED")
;; This is needed to allow msmtp to do its magic:
(setq message-sendmail-f-is-evil 't)
(setq message-sendmail-extra-arguments '("--read-envelope-from"))

(setq gnus-permanently-visible-groups ".*")


(setq gnus-thread-sort-functions
           '(gnus-thread-sort-by-most-recent-date))

;; (define-key gnus-group-mode-map (kbd "vo")
;;   '(lambda ()
;;      (interactive)
;;      (shell-command "offlineimap&" "*offlineimap*" nil))) 

 ;; (setq nnimap-split-inbox "INBOX") ;; (1)
 ;; (setq nnimap-split-predicate "UNDELETED") ;; (2)
 ;; (setq nnimap-split-rule
 ;;       '(
 ;;         ("INBOX.emacs" "^Subject:.*emacs")
 ;;         ("INBOX.Work" "^From:.*chnguyen@elcamino.edu")    
 ;;         ("INBOX.personal" "^To:.*you@personal.example.com")    
 ;;         ("INBOX.errors" "^From:.*\\(mailer.daemon\\|postmaster\\)")   
 ;;        )) 


(require 'gnus-desktop-notify)
(gnus-desktop-notify-mode)
(gnus-demon-add-scanmail)

;; Pretty Display html email
;; (setq mm-text-html-renderer 'w3m)
;; (setq gnus-inhibit-images nil)
;; (setq mm-inline-text-html-with-images "(setq gnus-mime-display-multipart-related-as-mixed nil)
(setq mm-text-html-renderer 'w3m)
(setq mm-inline-text-html-with-images t)
(setq mm-inline-text-html-with-w3m-keymap nil)
;(require 'w3m-e21t)

;; Organize My Life
;; (setq nnimap-split-methods
;;   '(("Elcamino.SI" "^From:.*Nguyen, Connie|^From:.*Barrueta, Luis")
;;     ("Elcamino.Library" "^From:*donotreply@overdrive.com")
;;     ("mail.other" "")))

(provide '.gnus)
;;; .gnus ends here
