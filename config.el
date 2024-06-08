;; Loads config while saving this for... stuff.
;; Place this in init.el
;; (load-file "~/.emacs.d/config.el")

;; Package Manager and Use package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Application specific settings
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

   (setq browse-url-browser-function 'browse-url-generic
	 browse-url-generic-program "/usr/bin/firefox-bin")

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (message "WSL")
  (setq visible-bell       nil
	ring-bell-function #'ignore)
  ;; Change the font size
  (add-to-list 'default-frame-alist
	       '(font . "DejaVu Sans Mono-18"))
  (setq
   browse-url-generic-program  "/home/jmendez/.local/bin/wsl-browse.sh"
   browse-url-browser-function #'browse-url-generic)

  )


;; Package Manager
  ;; (require 'notifications)
  ;; (setq alert-default-style 'notifier)
  ;; ;; OS Specific Stuff
  ;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/pdf-tools/")

;; Defun Section
(defun my-org-hook ()
  (flyspell-mode 1)
  (org-fragtog-mode 1)
  (visual-line-mode 1)
  )
(defun my-ledger-hook ()
  (setq-local tab-always-indent 'complete)
  (setq-local completion-cycle-threshold t)
  (setq-local ledger-complete-in-steps t))

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline 1)
  (display-line-numbers-mode)
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
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-11"))

(use-package fira-code-mode
  :config
  (fira-code-mode-set-font)
  (global-fira-code-mode)
  :custom
  (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  )

;; Save File
(setq delete-old-versions t)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Images in dired
(autoload 'iimage-mode "iimage" "Support Inline image minor mode." t)
(autoload 'turn-on-iimage-mode "iimage" "Turn on Inline image minor mode." t)
;; Eshell
(add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color")))


;; Major mode Hooks
(add-hook 'after-init-hook 'global-company-mode)

;; Personal Info and PIM Settings
(setq user-full-name "Jesse Mendez"
      user-mail-address "jmend46@lsu.edu")
(setq compose-mail-user-agent-warnings nil)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq calendar-latitude 30.4)
(setq calendar-longitude -91.18)
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

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

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Setup EBDB for contacts
(use-package ebdb
  :config
  (setq ebdb-mua-auto-update-p 'query)
  (setq ebdb-gnus-auto-update-p 'query)
  (setq edbd-default-window-size 0.2)
  (setq ebdb-sources "~/Dropbox/org/ebdb")
  (require 'ebdb-message)
  (require 'ebdb-gnus)
  (require 'ebdb-org))


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
     ("i" "Capture an idea to inbox" entry (file "~/Dropbox/org/inbox.org") "* %?\n")
     ("n" "Capture a next item" entry (file+headline "~/Dropbox/org/gtd.org" "Tasks") "* NEXT %?%^G\n")
     ))
  (org-directory "~/Dropbox/org")
  (org-agenda-custom-commands 
   '(
     ("c" "Computer" tags-todo "@computer-someday|@laptop-someday")
     ("e" "Errands" tags-todo "@errand-someday")
     ("p" "Phone" tags-todo "@phone-someday")
     ("o" "Internet" tags-todo "@online-someday")
     ("h" "Home" tags-todo "@home-someday")
     ("l" "LSU Campus" tags-todo "@campus-someday")
     ("s" "LArASIC Lab" tags-todo "@larasic-someday")
     ("r" "Research" tags-todo "@research-someday")
     ("w" "Homework" tags-todo "@homework-someday")
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

       (tags "someday+LEVEL=2")))))
  :config
  (setq org-agenda-files '("~/Dropbox/org/inbox.org"
			   "~/Dropbox/org/project.org"
			   "~/Dropbox/org/gtd.org"
			   "~/Dropbox/org/cal_calendar.org"))

  (setq org-refile-targets '((nil :maxlevel . 9)
			     ("~/Dropbox/org/someday.org" :maxlevel . 9)
			     ("~/Dropbox/org/gtd.org" :maxlevel . 3)
			     ("~/Dropbox/org/project.org" :maxlevel . 9)
			     ("~/Dropbox/org/cal_calendar.org" :maxlevel . 9)))
  ;; Looks
  (setq-default org-startup-indented t
		org-pretty-entities t
		org-use-sub-superscripts "{}"
		org-hide-emphasis-markers t
		org-startup-with-inline-images t
		org-image-actual-width '(300))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-allow-creating-parent-nodes t)					; Show full paths for refiling
  (setq org-outline-path-complete-in-steps nil)
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
  )

(use-package org-crypt
  :ensure nil
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  )

;; (use-package helm-bibtex
;;   :ensure t
;;   :config
;;   (setq bibtex-completion-bibliography "~/Dropbox/Library/main.bib"
;;         bibtex-completion-library-path "~/Dropbox/Library/pdf"
;;         bibtex-completion-pdf-field "File"
;;         bibtex-completion-notes-path "~/Dropbox/org/roam/reference")
;;   (setq bibtex-completion-pdf-symbol "⌘")
;;   (setq bibtex-completion-notes-symbol "✎")
;;   (setq bibtex-completion-additional-search-fields '(journal booktitle))
;;   (setq bibtex-completion-notes-extension ".org")
;;   (setq bibtex-completion-pdf-extension '(".pdf" ".djvu" ".txt"))
;;   ;; Para abrir URL/DOIs
;;   (setq bibtex-completion-browser-function
;;         (lambda (url _) (start-process "firefox" "*firefox*" "firefox" url)))

;;   (helm-add-action-to-source
;;    "Open annotated PDF (if present)" 'helm-bibtex-open-annotated-pdf
;;    helm-source-bibtex 1)
;;   :bind
;;   (("C-x C-b" . helm-bibtex)
;;    ("<menu>" . helm-bibtex)
;;    :map helm-command-map
;;    ("b" . helm-bibtex)))

;; Managing Bibliographies
(use-package bibtex
  :ensure nil
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to a document file." "" )))
  (bibtex-align-at-equal-sign t))

(use-package biblio)

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))
;; LaTeX previews
(use-package org-fragtog
  :after org
  :custom
  (org-startup-with-latex-preview t)
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; COnfiguring Website Publishing
(require 'ox-publish)
(setq org-html-htmlize-output-type 'css)
(setq org-publish-project-alist
      `(("pages"
         :base-directory "~/30-39_Education/32_Louisiana_State_University/32.47_website/org/"
         :base-extension "org"
					; Style Config
	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"https://gongzhitaao.org/orgcss/org.css\"/>"
					; HTML5
	 :html-doctype "html5"
	 :html-html5-fancy t
					; Disable some Org's HTML defaults
	 :html-head-include-scripts nil
	 :html-head-include-default-style nil
	 :html-preamble "
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
<style>
body {margin: 0;}

ul.topnav {
  list-style-type: none;
  margin: 0;
  padding: 0;
  overflow: hidden;
  background-color: #333;
}

ul.topnav li {float: left;}

ul.topnav li a {
  display: block;
  color: white;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
}

ul.topnav li a:hover:not(.active) {background-color: #111;}

ul.topnav li a.active {background-color: #04AA6D;}

ul.topnav li.right {float: right;}

@media screen and (max-width: 100%) {
  ul.topnav li.right, 
  ul.topnav li {float: none;}
}
</style>
<ul class=\"topnav\">
  <li><a href='/index.html'>Home</a></li>
  <li><a href='/test.html'>Test</a></li>
</ul>
"
	 :html-postamble "<hr/>
<footer>
  <div class=\"copyright-container\">
    <div class=\"copyright\">
      Copyright &copy; 2023-2024 Jesse Mendez some rights reserved<br/>
      Content is available under
      <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
        CC-BY-SA 4.0
      </a> unless otherwise noted
    </div>
    <div class=\"cc-badge\">
      <a rel=\"license\" href=\"http://creativecommons.org/licenses/by-sa/4.0/\">
        <img alt=\"Creative Commons License\"
             src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\" />
      </a>
    </div>
  </div>

  <div class=\"generated\">
    Created with %c on <a href=\"https://www.gnu.org\">GNU</a>/<a href=\"https://www.kernel.org/\">Linux</a>
  </div>
</footer>"
         :recursive t
         :publishing-directory "~/30-39_Education/32_Louisiana_State_University/32.47_website/html/"
         :publishing-function org-html-publish-to-html)

        ("static"
         :base-directory "~/30-39_Education/32_Louisiana_State_University/32.47_website/org/"
         :base-extension "css\\|txt\\|jpg\\|gif\\|png"
         :recursive t
         :publishing-directory  "~/30-39_Education/32_Louisiana_State_University/32.47_website/html/"
         :publishing-function org-publish-attachment)
("blog"
 :base-directory "~/30-39_Education/32_Louisiana_State_University/32.47_website/org/blog/"
 :base-extension "org"
 :publishing-directory "~/30-39_Education/32_Louisiana_State_University/32.47_website/html/blog/"
 :publishing-function org-html-publish-to-html

 :auto-sitemap t
 :sitemap-title "Blog Posts"
 :sitemap-filename "index.org"
 :sitemap-sort-files anti-chronologically)

("jessepmendez.com" :components ("pages" "blog" "static"))))
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
  (("C-c w b o" . citar-open)
   (:map org-mode-map
         :package org
         ("C-c w C". #'org-cite-insert))))


(use-package org-noter
  :ensure t
  )

(use-package denote
  :ensure t
  :custom
  (denote-directory "~/Dropbox/denote")
  (denote-sort-keywords t)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c d n" . denote-create-note)
   ("C-c d d" . denote-date)
   ("C-c d i" . denote-link-or-create)
   ("C-c d l" . denote-find-link)
   ("C-c d b" . denote-find-backlink)
   ("C-c d r" . denote-rename-file)
   ("C-c d R" . denote-rename-file-using-front-matter)
   ("C-c d k" . denote-keywords-add)
   ("C-c d K" . denote-keywords-remove))
  )

(use-package citar-denote
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t)
  (citar-notes-paths '("~/Dropbox/denote/"))
  
  :bind (("C-c b n" . citar-create-note)
         ("C-c b o" . citar-denote-open-note)
         ("C-c b f" . citar-denote-find-citation)
         ("C-c b d" . citar-denote-dwim)
         ("C-c b e" . citar-denote-open-reference-entry)
         ("C-c b a" . citar-denote-add-citekey)
         ("C-c b k" . citar-denote-remove-citekey)
         ("C-c b r" . citar-denote-find-reference)
         ("C-c b l" . citar-denote-link-reference)
         ("C-c b x" . citar-denote-nocite)
         ("C-c b y" . citar-denote-cite-nocite)))

;; Denote Explore
(use-package denote-explore
    :custom
    ;; Where to store network data and in which format
    (denote-explore-network-directory "~/Dropbox/denote/viz/")
    (denote-explore-network-filename "denote-network")
    (denote-explore-network-format 'graphviz)
    (denote-explore-network-graphviz-filetype "pdf")
    :bind
    (;; Statistics
     ("C-c w e c" . denote-explore-count-notes)
     ("C-c w e C" . denote-explore-count-keywords)
     ("C-c w e b" . denote-explore-keywords-barchart)
     ("C-c w e x" . denote-explore-extensions-barchart)
     ;; Random walks
     ("C-c w e r" . denote-explore-random-note)
     ("C-c w e l" . denote-explore-random-link)
     ("C-c w e k" . denote-explore-random-keyword)
     ;; Denote Janitor
     ("C-c w e d" . denote-explore-identify-duplicate-notes)
     ("C-c w e z" . denote-explore-zero-keywords)
     ("C-c w e s" . denote-explore-single-keywords)
     ("C-c w e o" . denote-explore-sort-keywords)
     ("C-c w e r" . denote-explore-rename-keywords)
     ;; Visualise denote
     ("C-c w e n" . denote-explore-network)
     ("C-c w e v" . denote-explore-network-regenerate)
     ("C-c w e D" . denote-explore-degree-barchart)))

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

