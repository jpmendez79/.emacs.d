;; Generic Settings (theoretically)
;; Package Manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)
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
(require 'ebdb-roam)
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
  :ensure t
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


(if (and (eq system-type 'gnu/linux)
         (getenv "WSLENV"))
    (load-file "~/.emacs.d/wsl.el")
  (load-file "~/.emacs.d/linux.el"))

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
     ("o" "Internet" tags-todo "@online-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "LSU Campus" tags-todo "@campus-someday")
     ("b" "BNL Onsite" tags-todo "@bnl-someday")
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
(setq org-agenda-files '("~/Dropbox/org/inbox.org" "~/Dropbox/org/someday.org" "~/Dropbox/org/project.org" "~/Dropbox/org/gtd.org"))
)
(setq org-refile-targets '((nil :maxlevel . 9)
			   (org-agenda-files :maxlevel . 9)
			   ("~/Dropbox/org/someday.org" :level . 2)
			   ("~/Dropbox/org/gtd.org" :level . 2)
			   ("~/Dropbox/org/project.org" :maxlevel . 1)
			   ("~/Dropbox/org/cal_calendar.org")))
(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
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


(use-package org-noter
  :ensure t
  )

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
 '(ebdb-sources "~/Dropbox/org/ebdb")
 '(package-selected-packages
   '(eat vterm org-noter ebdb org-roam-bibtex pdf-tools alert-toast gnus-desktop-notify company auctex magit org-fragtog use-package org-roam helm-bibtex eglot calfw-org calfw)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
