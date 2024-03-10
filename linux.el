;;; Linux specific Settings
(message "Linux")
;; Package Manager
(require 'notifications)
(setq alert-default-style 'notifier)
;; OS Specific Stuff
(add-to-list 'load-path "/usr/share/emacs/site-lisp/pdf-tools/")
;; Browser Settings
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/firefox-bin")

