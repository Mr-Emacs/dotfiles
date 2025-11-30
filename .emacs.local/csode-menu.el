;; Credit: Entire config (olivia-menu.el): By CodeVoid0x25
;; original repo: https://github.com/Codespace0x25/dotFiles/blob/main/.emacs.d/elpa/olivia/menu.el

(require 'ivy)

;; ----------------------------
;; Transparency helper
;; ----------------------------
(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; ============================================================
;; ðŸ”§ MENU REGISTRY SYSTEM (dynamic, runtime update friendly)
;; ============================================================

(defvar csode/menu-registry (make-hash-table :test 'equal)
  "Registry of menus. Keys are menu names, values are alists of (label . function).")

(defun csode/get-menu-options (name)
  (gethash name csode/menu-registry))

(defun csode/menu (title menu-name prev-fn)
  "Generic menu that looks up MENU-NAME in the registry dynamically."
  (let ((options (csode/get-menu-options menu-name)))
    (ivy-read title
              (append (mapcar #'car options) '("Back"))
              :action (lambda (choice)
                        (if (string= choice "Back")
                            (funcall prev-fn)
                          (funcall (cdr (assoc choice options))))))))

(defun csode/add-menu-option (menu label fn)
  "Add a single option to an existing MENU dynamically."
  (let ((opts (gethash menu csode/menu-registry)))
    (puthash menu (append opts (list (cons label fn))) csode/menu-registry)))

(defmacro csode/define-menu (name options)
  "Define and register a menu NAME with OPTIONS as (label . fn)."
  `(puthash ,name ,options csode/menu-registry))

;; ============================================================
;; ðŸ“¦ PACKAGE MANAGER REGISTRY (dynamic)
;; ============================================================

(defvar csode/package-managers nil
  "List of registered package managers for `csode/menu-packages`.
Each element is (NAME . FUNCTION).")

(defmacro csode/define-package-manager (name fn)
  "Define and register a package manager with NAME and FN."
  `(progn
     (add-to-list 'csode/package-managers (cons ,name ,fn) t)
     ,fn))

(defun csode/menu-packages ()
  "Dynamically select a package manager and run it."
  (interactive)
  (ivy-read "Packages: " (mapcar #'car csode/package-managers)
            :action (lambda (choice)
                      (let ((fn (cdr (assoc choice csode/package-managers))))
                        (when fn (funcall fn))))))

;; ============================================================
;; ðŸ’» PROJECTILE MENU
;; ============================================================

;; Projectile Menu
(csode/define-menu
 "projectile"
 '(("Switch Project" . projectile-switch-project)
   ("Find File" . projectile-find-file)
   ("Load Config" . (lambda ()
                      (let ((config-file (expand-file-name ".comfig.el" (projectile-project-root))))
                        (if (file-exists-p config-file)
                            (progn
                              (load-file config-file)
                              (message "Loaded config: %s" config-file))
                          (message "No .config.el found in project root.")))))
   ("Search in Project" . projectile-ripgrep)
   ("Compile Project" . projectile-compile-project)
   ("Eshell" . projectile-run-eshell)
   ("Kill Project Buffers" . projectile-kill-buffers)))


;; ============================================================
;; ðŸ“¦ XBPS MENU
;; ============================================================

(defun csode/xbps ()
  "Interactive XBPS menu."
  (interactive)
  (let* ((commands '(("Install" . "sudo xbps-install")
                     ("Update (XBPS)" . "sudo xbps-install -Syu")
                     ("Reinstall" . "sudo xbps-install")
                     ("Remove" . "sudo xbps-remove -R")
                     ("Search (XBPS)" . "xbps-query -Rs")
                     ("Search File (XBPS)" . "xbps-query -Rf")
                     ("Clean Orphans" . "xbps-remove -o")
                     ("Clean Cached" . "xbps-remove -y0")))
         (choice (ivy-read "xbps Command: " (mapcar #'car commands)))
         (command (cdr (assoc choice commands)))
         (pkg (if (string-match-p "Search\\|Install\\|Reinstall\\|Remove" choice)
                  (read-string "Package Name: "))))
    (let ((full-command (if (and pkg (not (string-empty-p pkg)))
                            (concat command " " pkg)
                          command)))
      (async-shell-command full-command "*XBPS*"))))

(csode/define-package-manager "XBPS" #'csode/xbps)

;; ============================================================
;; ROOT MENU + REGISTRY
;; ============================================================

(csode/define-menu
 "root"
 '(("M-x" . (lambda () (call-interactively #'execute-extended-command)))
   ("AppRun" . (lambda ()
                 (ivy-read "APP: "
                           (split-string (shell-command-to-string "compgen -c") "\n" t)
                           :action (lambda (cmd) (start-process cmd nil cmd)))))
   ("Web" . (lambda () (csode/menu "Web:" "web" #'csode/menu-root)))
   ("Shell" . (lambda () (csode/menu "Shell:" "shell" #'csode/menu-root)))
   ("Emacs" . (lambda () (csode/menu "Emacs:" "emacs" #'csode/menu-root)))
   ("Packages" . csode/menu-packages)
   ("Git" . (lambda () (csode/menu "Git:" "git" #'csode/menu-root)))
   ("Projectile" . (lambda () (csode/menu "Projectile:" "projectile" #'csode/menu-root)))
   ("TTS" . (lambda () (csode/menu "TTS:" "tts" #'csode/menu-root)))
   ("Close Buffer" . kill-buffer)))

;; ============================================================
;; REGISTER SUBMENUS (WEB, SHELL, EMACS, GIT, TTS)
;; ============================================================

(csode/define-menu
 "web"
 '(("Search" . (lambda ()
                 (csode/menu "Search:" "web-search"
                              #'(lambda () (csode/menu "Web:" "web" #'csode/menu-root)))))
   ("URL" . (lambda ()
              (csode/menu "URL:" "web-url"
                           #'(lambda () (csode/menu "Web:" "web" #'csode/menu-root)))))
   ("YouTube" . (lambda ()
                  (browse-url "https://www.youtube.com/")))
   ("Apple Music" . (lambda ()
                  (browse-url "https://beta.music.apple.com/")))))

(csode/define-menu
 "web-search"
 '(("DuckDuckGo" . (lambda ()
                     (ivy-read "Query: " (delete-dups csode/search-web-history)
                               :action (lambda (q)
                                         (add-to-list 'csode/search-web-history q)
                                         (browse-url (concat "https://duckduckgo.com/?q=" (url-hexify-string q)))))))
   ("YouTube" . (lambda ()
                  (ivy-read "Query: " (delete-dups csode/youtube-search-history)
                            :action (lambda (q)
                                      (add-to-list 'csode/youtube-search-history q)
                                      (browse-url (concat "https://www.youtube.com/results?search_query=" (url-hexify-string q)))))))
   ("AppleMusic" . (lambda ()
                     (ivy-read "Query: " (delete-dups csode/youtube-search-history)
                               :action (lambda (q)
					 (add-to-list 'csode/appleMusic-search-history q)
					 (browse-url
                       (concat "https://beta.music.apple.com/us/search?term="
                           (url-hexify-string q)))))))
   ("images" . (lambda ()
		 (ivy-read "Query: " (delete-dups csode/youtube-search-history)
			   :action (lambda (q)
                     (add-to-list 'csode/image-search-history q)
                     (browse-url
                      (concat "https://duckduckgo.com/?q="
                          (url-hexify-string q) "&ia=images&iax=images"))))))
      )
 )

(csode/define-menu
 "web-url"
 '(("HTTPS" . (lambda ()
                (ivy-read "HTTPS URL: " (delete-dups csode/url-https-history)
                          :action (lambda (url)
                                    (add-to-list 'csode/url-https-history url)
                                    (browse-url (concat "https://" url))))))
   ("HTTP" . (lambda ()
               (ivy-read "HTTP URL: " (delete-dups csode/url-http-history)
                         :action (lambda (url)
                                   (add-to-list 'csode/url-http-history url)
                                   (browse-url (concat "http://" url))))))))

;; Compile Menu
(csode/define-menu
 "shell"
 '(("Run Command" . (lambda ()
                      (ivy-read "Command: "
                                (split-string (shell-command-to-string "compgen -c") "\n" t)
                                :action (lambda (cmd) (start-process cmd nil cmd)))))
   ("pkill" . (lambda ()
                (ivy-read "Process: " nil
                          :action (lambda (proc)
                                    (let ((code (call-process "pkill" nil nil nil proc)))
                                      (when (/= code 0)
                                        (let ((pw (read-passwd "sudo password: ")))
                                          (with-temp-buffer
                                            (insert pw "\n")
                                            (call-process-region (point-min) (point-max)
                                                                 "sudo" nil nil nil "-S" "pkill" proc)))))))))
   ("Eshell" . (lambda () (eshell)))
   ("Paste" . (lambda () (term-paste)))))

;; Emacs Menu
(csode/define-menu
 "emacs"
 '(("Eval Menu" . (lambda () (call-interactively #'eval-expression)))
   ("Emoji" . ivy-emoji)
   ("LSP" . lsp)))

;; Git Menu
(csode/define-menu
 "git"
 '(("status" . (lambda () (async-shell-command "git status")))
   ("pull" . (lambda () (async-shell-command "git pull")))
   ("push" . (lambda () (async-shell-command "git push")))
   ("add" . (lambda ()
              (let ((f (read-file-name "File to add: ")))
                (async-shell-command (format "git add %s" (shell-quote-argument f))))))
   ("commit" . (lambda ()
                 (let ((m (read-string "Commit message: ")))
                   (async-shell-command (format "git commit -m %S" m)))))
   ("log" . (lambda () (async-shell-command "git log --oneline --graph --decorate")))))

;; TTS Menu
(csode/define-menu
 "tts"
 '(("Read Buffer" . (lambda ()
                      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                        (start-process "espeak" nil "espeak"  text))))
   ("Read Region" . (lambda ()
                      (if (use-region-p)
                          (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
                            (start-process "espeak" nil "espeak"  text))
                        (message "No region selected."))))))


(defun csode/M-x ()
  "Open Csode's root menu using Ivy."
  (interactive)
  (csode/menu "Menu:" "root" #'csode/M-x))

(provide 'csode-menu)
