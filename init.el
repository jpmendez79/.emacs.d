;; Generic Settings (theoretically)
;; Package Manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)
(if (and (eq system-type 'gnu/linux)
         (getenv "WSLENV"))
    (load-file "~/.emacs.d/wsl.el")
  (load-file "~/.emacs.d/linux.el"))

;; (setq use-package-verbose t)
(require 'use-package)
;; (setq load-prefer-newer t)

;; Defun Section
(defun my-org-hook ()
  (flyspell-mode 1)
  (org-fragtog-mode 1)
  (auto-fill-mode 1)
  )


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

;; Look and feel
(require 'notifications)
(display-time-mode 1)
(display-battery-mode 1)
(column-number-mode 1)

;; Save File
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(use-package vertico
    :init
    (vertico-mode)
    :custom
    (vertico-sort-function 'vertico-sort-history-alpha))

 (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides
     '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
  (use-package marginalia
    :init
    (marginalia-mode))


;; Major mode Hooks
(add-hook 'after-init-hook 'global-company-mode)

;; Personal Info and PIM Settings
(setq user-full-name "Jesse Mendez"
      user-mail-address "jmend46@lsu.edu")
(setq compose-mail-user-agent-warnings nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)

;; Setup EBDB for contacts
(setq ebdb-sources "~/Dropbox/org/ebdb")
(require 'ebdb-message)
(require 'ebdb-gnus)
(require 'ebdb-org)
(setq ebdb-mua-auto-update-p 'query)
(setq ebdb-gnus-auto-update-p 'query)
(setq edbd-default-window-size 0.2)

(use-package calfw
  :ensure t)

(use-package calfw-org
  :ensure t
  ;; :bind
  ;; ("M-<f3>" . cfw:open-org-calendar)
  :config
  ;; hotfix: incorrect time range display
  ;; source: https://github.com/zemaye/emacs-calfw/commit/3d17649c545423d919fd3bb9de2efe6dfff210fe
  )

(setq calendar-latitude 30.4)
(setq calendar-longitude -91.18)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

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


;; Images in dired
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
;; Eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))


;; Nov.el File associations
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Org Mode and various org packages
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (python . t)
     (plantuml . t)
     ))
  :hook (org-mode . my-org-hook)
  :custom
  (org-agenda-timegrid-use-ampm t)
  (org-agenda-include-diary t)
  (org-stuck-projects
   '("+Project-someday+LEVEL=1/-DONE-CANCELED-someday" ("NEXT" "WAITING")))
  (org-todo-keywords
   '((sequence "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
  (org-capture-templates
   '(
     ("a" "Capture an Appointment")
     ("ap" "Personal Calendar Appointment" entry (file  "~/Dropbox/org/cal_calendar.org" )
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
     ("o" "Internet" tags-todo "@online-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "LSU Campus" tags-todo "@campus-someday")
     ("r" "LArASIC Lab" tags-todo "@larasic-someday")
     ("m" "Personal Computer mercury" tags-todo "@mercury-someday")
     ("b" "Work Computer bortan" tags-todo "@bortan-someday")
     ("g" "Agendas" tags-todo "@agenda-someday")
     ("W" "Weekly Review"
      ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
       (tags "inbox")
       (tags-todo "-someday-inbox")					  ; type "l" in the agenda to review logged items
       (stuck "") ; review stuck projects as designated by org-stuck-projects
       (tags "Project-someday+LEVEL=1") ; review all projects (assuming you use todo keywords to designate projects)
       (tags-todo "-someday+TODO=\"WAITING\"")

       (tags "someday+LEVEL=2")))))) ; review waiting items
;; ...other commands here
;; (1) (2) (3) (4)
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
			 "~/Dropbox/org/project.org"
			 "~/Dropbox/org/gtd.org"
			 "~/Dropbox/org/cal_calendar.org"))

(setq org-refile-targets '((nil :maxlevel . 9)
			   ("~/Dropbox/org/someday.org" :maxlevel . 9)
			   ("~/Dropbox/org/gtd.org" :maxlevel . 3)
			   ("~/Dropbox/org/project.org" :maxlevel . 9)
			   ("~/Dropbox/org/cal_calendar.org" :maxlevel . 9)))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes t)					; Show full paths for refiling
(setq org-plantuml-exec-mode 'plantuml)
(setq org-plantuml-executable-path "/usr/bin/plantuml")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-c." 'org-time-stamp)
(global-set-key "\C-cp" 'org-pomodoro)
(global-set-key "\C-co" 'org-noter)
(global-set-key "\C-cu" 'org-reset-checkbox-state-subtree)

(setq org-tags-exclude-from-inheritance "project")

(setq org-todo-keywords
      '((sequence "NEXT(n)" "|" "DONE(d)" "Delegated(D)")
	(sequence "WAITING(w)" "APPT(a)" )
	(sequence "|" "CANCELED(c)")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))


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

;; Managing Bibliographies
  (use-package bibtex
    :custom
    (bibtex-dialect 'biblatex)
    (bibtex-user-optional-fields
     '(("keywords" "Keywords to describe the entry" "")
       ("file" "Link to a document file." "" )))
    (bibtex-align-at-equal-sign t))

(use-package biblio)

;; (use-package citar
;;   :custom
;;   (citar-library-paths '("~/Dropbox/Library/pdf/"))
;;   (citar-notes-paths '("~/Dropbox/org/roam/reference"))
;;   (citar-file-notes-extensions '("org"))
;;   ;; optional: org-cite-insert is also bound to C-c C-x C-@
;;   :bind
;;   (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
;;   )

  ;; Citar to access bibliographies
  (use-package citar
    :custom
    (org-cite-global-bibliography
     (directory-files "~/Dropbox/Library/" t
      "^[A-Z|a-z|0-9].+.bib$"))
    (citar-bibliography org-cite-global-bibliography)
    (org-cite-insert-processor 'citar)
    (org-cite-follow-processor 'citar)
    (org-cite-activate-processor 'citar)
    :bind
    (("C-c w c c" . citar-open)
     (:map org-mode-map
           :package org
           ("C-c w C". #'org-cite-insert))))

  (use-package citar-denote
    :config
    (citar-denote-mode)
    :custom
    (citar-open-always-create-notes t)
    :bind (("C-c w c n" . citar-create-note)
           ("C-c w c o" . citar-denote-open-note)
           ("C-c w c f" . citar-denote-find-citation)
           ("C-c w c d" . citar-denote-dwim)
           ("C-c w c e" . citar-denote-open-reference-entry)
           ("C-c w c a" . citar-denote-add-citekey)
           ("C-c w c k" . citar-denote-remove-citekey)
           ("C-c w c r" . citar-denote-find-reference)
           ("C-c w c l" . citar-denote-link-reference)
           ("C-c w c x" . citar-denote-nocite)
           ("C-c w c y" . citar-denote-cite-nocite)))

(use-package org-noter
  :ensure t
  )

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Dropbox/denote")
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote-create-note)
     ("C-c n d" . denote-date)
     ("C-c n i" . denote-link-or-create)
     ("C-c n l" . denote-find-link)
     ("C-c n b" . denote-find-backlink)
     ("C-c n r" . denote-rename-file)
     ("C-c n R" . denote-rename-file-using-front-matter)
     ("C-c n k" . denote-keywords-add)
     ("C-c n K" . denote-keywords-remove))
  )

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,"~/Dropbox/denote")))
  :bind
  (("C-c n f" . consult-notes)
   ("C-c n s" . consult-notes-search-in-all-notes)))


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


;; Utilities and Tools
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

  (setq lsp-tex-server 'digestif)

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

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "microboone"
   :token "xoxc-20528114481-4855123725845-6081064337895-871cdbe09f79d1e2f4b8116a21cafd79dc1c36051b4a992000c7c3e9f188984e"
   :cookie "xoxd-BIjqGeSvxFBE66JO%2FMIOFOqLBaV9fecjQfP8mhv1kjBtENldxjIt9%2B3f9CedUBzzOpnHAdBtzMpnyJ%2FNhptyTanmah2AnmKbeFGhfRKCSOqqEKQYu4bif%2FBp3U4Mk3CvePHv9tpL8j3A6JzZ%2BavwMrVw7N5TXUBUNeIoI90KRmonclY7Kqr8GDN%2FCE%2BJmHYj15WVVQjyaTk%3D"
   :default t
   :full-and-display-names t)

  (slack-register-team
   :name "cal-bridge"
   :token "xoxc-300413466707-5593888548706-6092677205685-a543cc74c3e21902fc6f94d0fa92e92763ea26da92a1b954ba277ec174073bb9"
   :cookie "xoxd-BIjqGeSvxFBE66JO%2FMIOFOqLBaV9fecjQfP8mhv1kjBtENldxjIt9%2B3f9CedUBzzOpnHAdBtzMpnyJ%2FNhptyTanmah2AnmKbeFGhfRKCSOqqEKQYu4bif%2FBp3U4Mk3CvePHv9tpL8j3A6JzZ%2BavwMrVw7N5TXUBUNeIoI90KRmonclY7Kqr8GDN%2FCE%2BJmHYj15WVVQjyaTk%3D"
   :full-and-display-names t)
  (slack-register-team
   :name "lsuheneutrino"
   :token "xoxc-2485187331220-4696739544550-6108238938305-2e08d2a74be956471e4722f038646c4dad4502135c5fbff4586428eded84d432"
   :cookie "xoxd-BIjqGeSvxFBE66JO%2FMIOFOqLBaV9fecjQfP8mhv1kjBtENldxjIt9%2B3f9CedUBzzOpnHAdBtzMpnyJ%2FNhptyTanmah2AnmKbeFGhfRKCSOqqEKQYu4bif%2FBp3U4Mk3CvePHv9tpL8j3A6JzZ%2BavwMrVw7N5TXUBUNeIoI90KRmonclY7Kqr8GDN%2FCE%2BJmHYj15WVVQjyaTk%3D"
   :full-and-display-names t)
  )


(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-command-extra-options " -shell-escape ")
 '(auth-source-save-behavior nil)
 '(calendar-holidays
   '((holiday-fixed 1 1 "New Year's Day")
     (holiday-float 1 1 3 "Martin Luther King Day")
     (holiday-fixed 2 2 "Groundhog Day")
     (holiday-fixed 2 14 "Valentine's Day")
     (holiday-float 2 1 3 "President's Day")
     (holiday-fixed 3 17 "St. Patrick's Day")
     (holiday-fixed 4 1 "April Fools' Day")
     (holiday-float 5 0 2 "Mother's Day")
     (holiday-float 5 1 -1 "Memorial Day")
     (holiday-fixed 6 14 "Flag Day")
     (holiday-float 6 0 3 "Father's Day")
     (holiday-fixed 7 4 "Independence Day")
     (holiday-float 9 1 1 "Labor Day")
     (holiday-float 10 1 2 "Columbus Day")
     (holiday-fixed 10 31 "Halloween")
     (holiday-fixed 11 11 "Veteran's Day")
     (holiday-float 11 4 4 "Thanksgiving")
     (holiday-easter-etc)
     (holiday-fixed 12 25 "Christmas")
     (if calendar-christian-all-holidays-flag
	 (append
	  (holiday-fixed 1 6 "Epiphany")
	  (holiday-julian 12 25 "Christmas (Julian calendar)")
	  (holiday-greek-orthodox-easter)
	  (holiday-fixed 8 15 "Assumption")
	  (holiday-advent 0 "Advent")))
     (if calendar-hebrew-all-holidays-flag
	 (append
	  (holiday-hebrew-tisha-b-av)
	  (holiday-hebrew-misc)))
     (holiday-islamic-new-year)
     (holiday-islamic 9 1 "Ramadan Begins")
     (if calendar-islamic-all-holidays-flag
	 (append
	  (holiday-islamic 1 10 "Ashura")
	  (holiday-islamic 3 12 "Mulad-al-Nabi")
	  (holiday-islamic 7 26 "Shab-e-Mi'raj")
	  (holiday-islamic 8 15 "Shab-e-Bara't")
	  (holiday-islamic 9 27 "Shab-e Qadr")
	  (holiday-islamic 10 1 "Id-al-Fitr")
	  (holiday-islamic 12 10 "Id-al-Adha")))
     (if calendar-bahai-all-holidays-flag
	 (append
	  (holiday-fixed 11 26 "Day of the Covenant")
	  (holiday-fixed 11 28 "Ascension of `Abdu’l-Bahá")))
     (holiday-chinese-new-year)
     (if calendar-chinese-all-holidays-flag
	 (append
	  (holiday-chinese 1 15 "Lantern Festival")
	  (holiday-chinese-qingming)
	  (holiday-chinese 5 5 "Dragon Boat Festival")
	  (holiday-chinese 7 7 "Double Seventh Festival")
	  (holiday-chinese 8 15 "Mid-Autumn Festival")
	  (holiday-chinese 9 9 "Double Ninth Festival")
	  (holiday-chinese-winter-solstice)))
     (solar-equinoxes-solstices)
     (holiday-sexp calendar-daylight-savings-starts
		   (format "Daylight Saving Time Begins %s"
			   (solar-time-string
			    (/ calendar-daylight-savings-starts-time
			       (float 60))
			    calendar-standard-time-zone-name)))
     (holiday-sexp calendar-daylight-savings-ends
		   (format "Daylight Saving Time Ends %s"
			   (solar-time-string
			    (/ calendar-daylight-savings-ends-time
			       (float 60))
			    calendar-daylight-time-zone-name)))))
 '(connection-local-criteria-alist
   '(((:application eshell :protocol "ssh" :machine "uboonepro")
      autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"uboonepro\"\))
     ((:machine "uboonepro")
      uboonepro-vars)
     ((:application eshell :protocol "ssh" :machine "workstation")
      autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"workstation\"\))
     ((:machine "workstation")
      workstation-vars)
     ((:application eshell)
      eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"uboonepro\"\)
      (eshell-path-env-list "/usr/bin" "/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"))
     (uboonepro-vars
      (company-gtags--executable-connection))
     (autogenerated-connection-local-profile/\(:application\ eshell\ :protocol\ \"ssh\"\ :machine\ \"workstation\"\)
      (eshell-path-env-list "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"))
     (workstation-vars
      (company-gtags--executable-connection))
     (eshell-connection-default-profile
      (eshell-path-env-list))
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
      (null-device . "/dev/null"))))
 '(ebdb-sources "~/Dropbox/org/ebdb")
 '(org-agenda-files
   '("/mnt/c/Users/jesse/Dropbox/org/research.org" "/home/jmendez/Dropbox/org/inbox.org" "/home/jmendez/Dropbox/org/project.org" "/home/jmendez/Dropbox/org/gtd.org" "/home/jmendez/Dropbox/org/cal_calendar.org"))
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(citar-denote consult-notes marginalia orderless vertico denote org-cliplink org-gcal slack deft eat org-noter ebdb pdf-tools alert-toast gnus-desktop-notify company auctex magit org-fragtog use-package helm-bibtex eglot calfw-org calfw)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
