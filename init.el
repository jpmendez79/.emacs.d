;;; Emacs Init File

;; Package Manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq use-package-verbose t)
(require 'use-package)
(setq load-prefer-newer t)
(add-to-list 'load-path "/usr/share/emacs/site-lisp/pdf-tools/")
;; Defun Section
(defun my-org-hook ()
  (flyspell-mode 1)
  (org-fragtog-mode 1)
  )

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)
  (setq exwm-workspace-display-echo-area-timeout 3)
  ;; Launch apps that will run in the background
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueberry-tray")
  (efs/run-in-background "davmail")
  (efs/run-in-background "slack -u")
  (efs/run-in-background "wpa_gui -t"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

;; (defun pinentry-emacs (desc prompt ok error)
;;   (let ((str (read-passwd
;; 	      (concat (replace-regexp-in-string "%22" "\""
;; 						(replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
;;     str))

(defun my-ledger-hook ()
  (setq-local tab-always-indent 'complete)
  (setq-local completion-cycle-threshold t)
  (setq-local ledger-complete-in-steps t))

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline 1)
  ;; (flycheck-prog-mode 1)
  )


(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

;; Major mode Hooks
(add-hook 'after-init-hook 'global-company-mode)

;; Personal Info and PIM Settings
(setq user-full-name "Jesse Mendez"
      user-mail-address "jmend46@lsu.edu")
(setq compose-mail-user-agent-warnings nil)
(setq send-mail-function 'sendmail-send-it
      sendmail-program "/usr/bin/msmtp"
      mail-specify-envelope-from t
      message-sendmail-envelope-from 'header
      mail-envelope-from 'header)
(setq message-send-mail-function 'message-send-mail-with-sendmail)


;; Setup EBDB for contacts
(require 'ebdb-wl)
(require 'ebdb-message)
(setq ebdb-mua-auto-update-p 'query)
(setq ebdb-gnus-auto-update-p 'query)
(setq edbd-default-window-size 0.2)

;; ;; Wanderlust Email
;; (use-package wl
;;   :config
;; )

;; Mu4e
(require 'mu4e)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Personal"
	  :enter-func (lambda () (mu4e-message "Entering Personal context"))
          :leave-func (lambda () (mu4e-message "Leaving Personal context"))
          :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/personal" (mu4e-message-field msg :maildir))))
	  :vars '(
                  (user-full-name . "Jesse Mendez")
                  (user-mail-address . "jessepmendez79@gmail.com")
                  (mu4e-compose-signature . (concat
                                             "Jesse Mendez"))
		  (mu4e-drafts-folder . "/personal/\[Gmail\]/Drafts")
		  (mu4e-sent-folder . "/personal/\[Gmail\]/Sent Mail")
		  (mu4e-trash-folder . "/personal/\[Gmail\]/Trash")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
		  ))
	,(make-mu4e-context
          :name "Louisiana State University"
          :match-func (lambda (msg)
			(when msg
			  (string-match-p "^/lsu" (mu4e-message-field msg :maildir))))
	  :enter-func (lambda () (mu4e-message "Entering LSU context"))
          :leave-func (lambda () (mu4e-message "Leaving LSU context"))
          :vars '(
                  (user-full-name . "Jesse Mendez")
                  (user-mail-address . "jmend46@lsu.edu")
                  (mu4e-compose-signature . (concat
                                             "Jesse Mendez"))
                  (mu4e-sent-folder . "/lsu/Sent")
                  (mu4e-drafts-folder . "/lsu/Drafts")
                  (mu4e-trash-folder .  "/lsu/Trash")
                  (mu4e-refile-folder . "/lsu/Archive")
		  (smtpmail-smtp-server  . "127.0.0.1")
                  (smtpmail-smtp-service . 1025)
                  (smtpmail-stream-type  . plain)

                  ;; (mu4e-maildir-shortcuts . (("/INBOX" . ?i)
                  ;;                            ("/Archive" . ?a)
                  ;;                            ("/Sent" . ?s)
                  ;;                            ("/Trash" . ?t)
		  ;; 			     ("/Junk" . ?j)))
		  ))
	))
(setq mu4e-change-filenames-when-moving t)
(setq mu4e-context-policy 'pick-first)
(setq message-send-mail-function 'smtpmail-send-it)
(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
;; don't save messages to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)
;; location Settings 
(setq calendar-latitude 30.4)
(setq calendar-longitude -91.18)

;; Save File
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Notifications
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))


;; Look and feel
(require 'notifications)
(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)

;; Utilities and Tools

;; Tex and Latex Settings
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'TeX-mode-hook #'eglot-ensure)

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :pin manual ;; don't reinstall when package updates
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (require 'pdf-occur)
  (pdf-tools-install :no-query))

;; Browser Settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox-bin")

;; Nov.el File associations
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


;; Images in dired
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)

;; Eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))

;; Org Mode and various org packages
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (python . t)
     ))
  :hook (org-mode . my-org-hook)
  :custom
  (org-agenda-timegrid-use-ampm t)
  (org-stuck-projects
   '("+Project-someday+LEVEL=1/-DONE-CANCELED-someday" ("NEXT" "WAITING")))
  (org-todo-keywords
   '((sequence "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates
   '(
     ("a" "Capture an Appointment")
     ("ap" "Personal Calendar Appointment" entry (file  "~/Dropbox/org/cal_personal.org" )
      "* %?\n\n:PROPERTIES:\n\n:END:\n\n")
     ("as" "School Calendar Appointment" entry (file  "~/Dropbox/org/cal_school.org" )
      "* %?\n\n:PROPERTIES:\n\n:END:\n\n")
     ("i" "Capture an idea to inbox" entry (file "~/Dropbox/org/inbox.org") "* %?\n")))
  (org-directory "~/Dropbox/org")
  (org-agenda-custom-commands 
   '(("n" "Anywhere" tags-todo "@anywhere-someday")
     ("c" "Computer" tags-todo "@computer-someday|@laptop-someday")
     ("e" "Errands" tags-todo "@errand-someday")
     ("p" "Phone" tags-todo "@phone-someday")
     ("i" "Internet" tags-todo "@internet-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "LSU Campus" tags-todo "@campus-someday")
     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
       (tags "inbox")
       (tags-todo "-someday-inbox")					  ; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (tags "Project-someday+LEVEL=2") ; review all projects (assuming you use todo keywords to designate projects)
       (todo "WAITING")

       (tags "someday+LEVEL=2")))))) ; review waiting items
;; ...other commands here
;; (1) (2) (3) (4)

(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)
			   ("/home/random/Dropbox/org/someday.org" :level . 2)
			   ("/home/random/Dropbox/org/gtd.org" :level . 2)
			   ("/home/random/Dropbox/org/project.org" :maxlevel . 1)))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path 'file)                  ; Show full paths for refiling

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c." 'org-time-stamp)
(global-set-key "\C-cp" 'org-pomodoro)
(global-set-key "\C-co" 'org-noter)
(setq org-tags-exclude-from-inheritance "project")

(setq org-todo-keywords
      '((sequence "NEXT(n)" "|" "DONE(d)" "Delegated(D)")
	(sequence "WAITING(w)" "APPT(a)" )
	(sequence "|" "CANCELED(c)")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :custom
  (org-roam-directory "~/Dropbox/org")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:50}" 'face 'org-tag)))

  (org-roam-capture-templates
   '(("m" "main" plain
      "%?"
      :if-new (file+head "${slug}.org"
			 "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("r" "reference" plain "%?"
      :if-new
      (file+head "${title}.org" "#+title: ${title}\n")
      :immediate-finish t
      :unnarrowed t)
     ("b" "bibliography reference" plain
      (file "~/Dropbox/org/bib-roam-template.org")
      :target
      (file+head "reference/${citekey}.org" "#+title: ${title}\n"))
     ("a" "article" plain "%?"
      :if-new
      (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :articlpe:\n")
      :immediate-finish t
      :unnarrowed t)))
  :config
  (org-roam-setup)
  (org-roam-bibtex-mode +1)
  )

(use-package org-roam-bibtex
  :after (org-roam)
  :bind (:map org-mode-map ("C-c n b" . orb-note-actions))
  :config
  ;; (setq orb-note-actions-interface 'hydra)
  (setq orb-preformat-keywords
        '("citekey" "title" "url" "author-or-editor" "keywords" "file" "date")
        orb-process-file-keyword t
        orb-insert-interface 'helm-bibtex 
        orb-file-field-extensions '("pdf")))

(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography "~/Dropbox/Library/main.bib"
        bibtex-completion-library-path "~/Dropbox/Library/pdf"
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path "~/Dropbox/org/roam/reference")
  (setq bibtex-completion-pdf-symbol "⌘")
  (setq bibtex-completion-notes-symbol "✎")
  (setq bibtex-completion-additional-search-fields '(journal booktitle))


  (setq bibtex-completion-notes-extension ".org")
  (setq bibtex-completion-pdf-extension '(".pdf" ".djvu" ".txt"))

  ;; Para abrir URL/DOIs
  (setq bibtex-completion-browser-function
        (lambda (url _) (start-process "firefox" "*firefox*" "firefox" url)))

  (helm-add-action-to-source
   "Open annotated PDF (if present)" 'helm-bibtex-open-annotated-pdf
   helm-source-bibtex 1)
  :bind
  (("C-x C-b" . helm-bibtex)
   ("<menu>" . helm-bibtex)
   :map helm-command-map
   ("b" . helm-bibtex)))

(use-package citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))

(require 'ox-beamer)
(require 'ox-latex)
(setq org-export-allow-bind-keywords t)
(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"))
(setq org-latex-minted-options '(("breaklines" "true")
				 ("breakanywhere" "true")))
(setq org-latex-pdf-process
      (mapcar
       (lambda (s)
	 (replace-regexp-in-string "%latex " "%latex -shell-escape " s))
       org-latex-pdf-process))


(use-package org-noter
  :ensure t
  )
(use-package citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/Dropbox/Library/master.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths '("~/Dropbox/Library/pdf/"))
  (citar-notes-paths '("~/Dropbox/org/roam/reference"))
  (citar-file-notes-extensions '("org"))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  )

;; Programming Settings
(setq c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(other . "linux")))
(setq-default c-electric-flag t)
(setq c-toggle-electric-state 1)
(add-hook 'c-mode-hook 'c-toggle-auto-newline 1)
(add-hook 'emacs-lisp-mode-hook 'electric-pair-mode)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; Eglot Server
(use-package eglot
  :ensure t
  :config
  ;; (setq lsp-tex-server 'digestif)

  )

(use-package pyvenv
  :ensure t
  :init
  (setenv "WORKON_HOME" "~/.virtualenvs/")
  :config
  ;; Set correct Python1 interpreter
  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python")))))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python")))))


(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; Ledger mode
(use-package ledger-mode
  :hook (ledger-mode . my-ledger-hook)
  )

;; EXWM Settings
(use-package exwm
  :ensure t
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  (require 'exwm-randr)
  (exwm-randr-enable)

  ;; Load the system tray before exwm-init
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 22)
  (exwm-systemtray-enable)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
	'(?\C-x
	  ?\C-u
	  ?\C-h
	  ?\M-x
	  ?\M-`
	  ?\M-&
	  ?\M-:
	  ?\C-\M-j  ;; Buffer list
	  ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
	`(
	  ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
	  ([?\s-r] . exwm-reset)

	  ;; Move between windows
	  ([s-left] . windmove-left)
	  ([s-right] . windmove-right)
	  ([s-up] . windmove-up)
	  ([s-down] . windpmove-down)

	  ;; Launch applications via shell command
	  ([?\s-&] . (lambda (command)
		       (interactive (list (read-shell-command "$ ")))
		       (start-process-shell-command command nil command)))
	  ;; Switch workspace
	  ([?\s-w] . exwm-workspace-switch)
	  ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
	  ([?\s-m] . exwm-workspace-move-window)
	  ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
	  ,@(mapcar (lambda (i)
		      `(,(kbd (format "s-%d" i)) .
			(lambda ()
			  (interactive)
			  (exwm-workspace-switch-create ,i))))
		    (number-sequence 0 9))))

  (require 'ido)
  (ido-mode 1)
  ;; let's get encryption established
  (require 'epg)
  (setq epg-pinentry-mode 'loopback)
  (exwm-enable))
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-extra-options " -shell-escape ")
 '(auth-source-save-behavior nil)
 '(connection-local-criteria-alist
   '(((:machine
       #("mercury" 0 7
	 (tramp-default t)))
      mercury-vars)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((mercury-vars
      (company-gtags--executable-connection))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(ebdb-add-mails 'query)
 '(ebdb-ignore-header-alist
   '((sender . "Reply")
     (sender . "*reply*")
     (sender . "*Rewards*")))
 '(eshell-prompt-function
   '(lambda nil
      (concat
       (shortened-path
	(eshell/pwd)
	40)
       (if
	   (=
	    (user-uid)
	    0)
	   " # " " λ "))))
 '(eshell-prompt-regexp "^[^#λ\12]* [#λ] ")
 '(newsticker-url-list
   '(("The Advocate Baton Rouge News" "https://morss.it/theadvocate.com/search/?q=&t=article&l=35&d=&d1=&d2=&s=start_time&sd=desc&c%5b%5d=baton_rouge/news*,baton_rouge/opinion*,baton_rouge/sports*,new_orleans/sports/saints&nk=%23tncen&f=rss" nil nil nil)))
 '(org-agenda-files
   '("~/Dropbox/org/cal_calendar.org" "/home/random/Dropbox/org/project.org" "/home/random/Dropbox/org/cal_school.org" "/home/random/Dropbox/org/inbox.org" "/home/random/Dropbox/org/classes.org" "/home/random/Dropbox/org/someday.org" "/home/random/Dropbox/org/gtd.org" "/home/random/Dropbox/org/cal_personal.org"))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-id ol-info ol-irc ol-mhe ol-rmail ol-w3m ol-wl))
 '(org-stuck-projects
   '("+Project-someday+LEVEL=1/-DONE-CANCELED-someday"
     ("NEXT" "WAITING")))
 '(package-selected-packages
   '(org-ql cern-root-mode biblio-core bibtex-completion helm-bibtex khoj dired-hide-dotfiles org-drill rainbow-delimiters company wanderlust 0blayout multiple-cursors transient org-contrib org-roam-ui djvu org-roam org-roam-bibtex kaolin-themes ebdb auctex ledger-mode org-fragtog vdirel pass oauth2-request simple-httpd laas yasnippet-snippets vterm pyvenv python-mode csv-mode circadian spacemacs-theme xterm-color exwm org-pomodoro magit org-ref org-noter smtpmail-multi arduino-mode flycheck oauth oauth2 async use-package org-plus-contrib)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-prompt ((t (:foreground "deep sky blue" :underline t :slant oblique :weight bold)))))
