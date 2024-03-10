(if (and (eq system-type 'gnu/linux)
         (getenv "WSLENV"))
    (load-file "~/.emacs.d/wsl.el")
  (load-file "~/.emacs.d/linux.el"))


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
 '(org-agenda-files
   '("/home/jmendez/Dropbox/org/cal_calendar.org" "/home/jmendez/Dropbox/org/inbox.org" "/home/jmendez/Dropbox/org/someday.org" "/home/jmendez/Dropbox/org/project.org" "/home/jmendez/Dropbox/org/gtd.org"))
 '(package-selected-packages
   '(eat vterm org-noter ebdb org-roam-bibtex pdf-tools alert-toast gnus-desktop-notify company auctex magit org-fragtog use-package org-roam helm-bibtex eglot calfw-org calfw)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
