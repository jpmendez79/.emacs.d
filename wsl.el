;; WSL specific settings
(message "WSL")

;; The bell -__-
(setq visible-bell       nil
      ring-bell-function #'ignore)

;; Change the font size
(add-to-list 'default-frame-alist
             '(font . "DejaVu Sans Mono-12"))

;; Teach Emacs how to open links in your default Windows browser
;; (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
;;       (cmd-args '("/c" "start")))
;;   (when (file-exists-p cmd-exe)
;;     (setq browse-url-generic-program  cmd-exe
;;           browse-url-generic-args     cmd-args
;;           browse-url-browser-function 'browse-url-generic
;;           search-web-default-browser 'browse-url-generic)))
 (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic)
