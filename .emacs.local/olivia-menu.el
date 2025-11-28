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

(defvar olivia/menu-registry (make-hash-table :test 'equal)
  "Registry of menus. Keys are menu names, values are alists of (label . function).")

(defun olivia/get-menu-options (name)
  (gethash name olivia/menu-registry))

(defun olivia/menu (title menu-name prev-fn)
  "Generic menu that looks up MENU-NAME in the registry dynamically."
  (let ((options (olivia/get-menu-options menu-name)))
    (ivy-read title
              (append (mapcar #'car options) '("Back"))
              :action (lambda (choice)
                        (if (string= choice "Back")
                            (funcall prev-fn)
                          (funcall (cdr (assoc choice options))))))))

(defun olivia/add-menu-option (menu label fn)
  "Add a single option to an existing MENU dynamically."
  (let ((opts (gethash menu olivia/menu-registry)))
    (puthash menu (append opts (list (cons label fn))) olivia/menu-registry)))

(defmacro olivia/define-menu (name options)
  "Define and register a menu NAME with OPTIONS as (label . fn)."
  `(puthash ,name ,options olivia/menu-registry))

;; ============================================================
;; ðŸ“¦ PACKAGE MANAGER REGISTRY (dynamic)
;; ============================================================

(defvar olivia/package-managers nil
  "List of registered package managers for `olivia/menu-packages`.
Each element is (NAME . FUNCTION).")

(defmacro olivia/define-package-manager (name fn)
  "Define and register a package manager with NAME and FN."
  `(progn
     (add-to-list 'olivia/package-managers (cons ,name ,fn) t)
     ,fn))

(defun olivia/menu-packages ()
  "Dynamically select a package manager and run it."
  (interactive)
  (ivy-read "Packages: " (mapcar #'car olivia/package-managers)
            :action (lambda (choice)
                      (let ((fn (cdr (assoc choice olivia/package-managers))))
                        (when fn (funcall fn))))))

;; ============================================================
;; ðŸ’» PROJECTILE MENU
;; ============================================================

;; Projectile Menu
(olivia/define-menu
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
;; ðŸ“¦ PACMAN / YAY MENU
;; ============================================================

(defun olivia/pacman ()
  "Interactive Pacman/Yay menu."
  (interactive)
  (let* ((commands '(("Install" . "sudo yay -S")
                     ("Update (Pacman)" . "sudo pacman -Syu")
                     ("Update (Yay)" . "yay -Syu")
                     ("Reinstall" . "sudo pacman -S --reinstall")
                     ("Remove" . "sudo pacman -Rns")
                     ("Search (Pacman)" . "pacman -Ss")
                     ("Search (Yay)" . "yay -Ss")
                     ("Clean Cache" . "sudo pacman -Sc")
                     ("Clean Orphans" . "sudo pacman -Rns $(pacman -Qdtq)")))
         (choice (ivy-read "Pacman/Yay Command: " (mapcar #'car commands)))
         (command (cdr (assoc choice commands)))
         (pkg (if (string-match-p "Search\\|Install\\|Reinstall\\|Remove" choice)
                  (read-string "Package Name: "))))
    (let ((full-command (if (and pkg (not (string-empty-p pkg)))
                            (concat command " " pkg)
                          command)))
      (async-shell-command full-command "*Pacman Output*"))))

(olivia/define-package-manager "Pacman" #'olivia/pacman)

;; ============================================================
;; ðŸ“¦ ELPA MENU
;; ============================================================

(defun olivia/elpa ()
  "Interactive ELPA menu with install, remove, update, etc."
  (interactive)
  (let* ((commands
          '(("Install" . (lambda ()
                           (ivy-read "Install Package: "
                                     (mapcar #'symbol-name (mapcar #'car package-archive-contents))
                                     :action (lambda (pkg)
                                               (package-install (intern pkg))
                                               (message "Installed: %s" pkg)))))
            ("Remove" . (lambda ()
                          (ivy-read "Remove Package: "
                                    (mapcar #'symbol-name (mapcar #'car package-alist))
                                    :action (lambda (pkg)
                                              (package-delete (cadr (assoc (intern pkg) package-alist)))
                                              (message "Removed: %s" pkg)))))
            ("Update All" . (lambda ()
                              (package-refresh-contents)
                              (package-menu-mark-upgrades)
                              (package-menu-execute t)
                              (message "Upgraded all packages.")))
            ("Describe Package" . (lambda ()
                                    (ivy-read "Describe Package: "
                                              (mapcar #'symbol-name (mapcar #'car package-archive-contents))
                                              :action (lambda (pkg)
                                                        (describe-package (intern pkg))))))
            ("List Installed" . (lambda () (list-packages t))))))
    (ivy-read "ELPA Command: " (mapcar #'car commands)
              :action (lambda (c) (funcall (cdr (assoc c commands)))))))

(olivia/define-package-manager "Elpa" #'olivia/elpa)

;; ============================================================
;; HISTORIES
;; ============================================================

(defvar olivia/search-web-history nil)
(defvar olivia/image-web-history nil)
(defvar olivia/youtube-search-history nil)
(defvar olivia/appleMusic-search-history nil)
(defvar olivia/url-https-history nil)
(defvar olivia/url-http-history nil)

(defun olivia/save-histories ()
  (with-temp-file "~/.emacs.d/olivia-histories.el"
    (insert (format "(setq olivia/search-web-history '%S)\n" olivia/search-web-history))
    (insert (format "(setq olivia/image-web-history '%S)\n" olivia/search-web-history))
    (insert (format "(setq olivia/youtube-search-history '%S)\n" olivia/youtube-search-history))
    (insert (format "(setq olivia/appleMusic-search-history '%S)\n" olivia/youtube-search-history))
    (insert (format "(setq olivia/url-https-history '%S)\n" olivia/url-https-history))
    (insert (format "(setq olivia/url-http-history '%S)\n" olivia/url-http-history))))
(add-hook 'kill-emacs-hook 'olivia/save-histories)

(defun olivia/load-histories ()
  (load "~/.emacs.d/olivia-histories.el" t))
(add-hook 'emacs-startup-hook 'olivia/load-histories)

;; ============================================================
;; ROOT MENU + REGISTRY
;; ============================================================

(olivia/define-menu
 "root"
 '(("M-x" . (lambda () (call-interactively #'execute-extended-command)))
   ("AppRun" . (lambda ()
                 (ivy-read "APP: "
                           (split-string (shell-command-to-string "compgen -c") "\n" t)
                           :action (lambda (cmd) (start-process cmd nil cmd)))))
   ("Web" . (lambda () (olivia/menu "Web:" "web" #'olivia/menu-root)))
   ("Shell" . (lambda () (olivia/menu "Shell:" "shell" #'olivia/menu-root)))
   ("Emacs" . (lambda () (olivia/menu "Emacs:" "emacs" #'olivia/menu-root)))
   ("Packages" . olivia/menu-packages)
   ("Git" . (lambda () (olivia/menu "Git:" "git" #'olivia/menu-root)))
   ("Projectile" . (lambda () (olivia/menu "Projectile:" "projectile" #'olivia/menu-root)))
   ("TTS" . (lambda () (olivia/menu "TTS:" "tts" #'olivia/menu-root)))
   ("Close Buffer" . kill-buffer)))

;; ============================================================
;; REGISTER SUBMENUS (WEB, SHELL, EMACS, GIT, TTS)
;; ============================================================

(olivia/define-menu
 "web"
 '(("Search" . (lambda ()
                 (olivia/menu "Search:" "web-search"
                              #'(lambda () (olivia/menu "Web:" "web" #'olivia/menu-root)))))
   ("URL" . (lambda ()
              (olivia/menu "URL:" "web-url"
                           #'(lambda () (olivia/menu "Web:" "web" #'olivia/menu-root)))))
   ("YouTube" . (lambda ()
                  (browse-url "https://www.youtube.com/")))
   ("Apple Music" . (lambda ()
                  (browse-url "https://beta.music.apple.com/")))))

(olivia/define-menu
 "web-search"
 '(("DuckDuckGo" . (lambda ()
                     (ivy-read "Query: " (delete-dups olivia/search-web-history)
                               :action (lambda (q)
                                         (add-to-list 'olivia/search-web-history q)
                                         (browse-url (concat "https://duckduckgo.com/?q=" (url-hexify-string q)))))))
   ("YouTube" . (lambda ()
                  (ivy-read "Query: " (delete-dups olivia/youtube-search-history)
                            :action (lambda (q)
                                      (add-to-list 'olivia/youtube-search-history q)
                                      (browse-url (concat "https://www.youtube.com/results?search_query=" (url-hexify-string q)))))))
   ("AppleMusic" . (lambda ()
                     (ivy-read "Query: " (delete-dups olivia/youtube-search-history)
                               :action (lambda (q)
					 (add-to-list 'olivia/appleMusic-search-history q)
					 (browse-url
				       (concat "https://beta.music.apple.com/us/search?term="
					       (url-hexify-string q)))))))
   ("images" . (lambda ()
		 (ivy-read "Query: " (delete-dups olivia/youtube-search-history)
			   :action (lambda (q)
				     (add-to-list 'olivia/image-search-history q)
				     (browse-url
				      (concat "https://duckduckgo.com/?q="
					      (url-hexify-string q) "&ia=images&iax=images"))))))
      )
 )

(olivia/define-menu
 "web-url"
 '(("HTTPS" . (lambda ()
                (ivy-read "HTTPS URL: " (delete-dups olivia/url-https-history)
                          :action (lambda (url)
                                    (add-to-list 'olivia/url-https-history url)
                                    (browse-url (concat "https://" url))))))
   ("HTTP" . (lambda ()
               (ivy-read "HTTP URL: " (delete-dups olivia/url-http-history)
                         :action (lambda (url)
                                   (add-to-list 'olivia/url-http-history url)
                                   (browse-url (concat "http://" url))))))))

;; Shell Menu
(olivia/define-menu
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
(olivia/define-menu
 "emacs"
 '(("Eval Menu" . (lambda () (call-interactively #'eval-expression)))
   ("Emoji" . ivy-emoji)
   ("LSP" . lsp)))

;; Git Menu
(olivia/define-menu
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
(olivia/define-menu
 "tts"
 '(("Read Buffer" . (lambda ()
                      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
                        (start-process "espeak" nil "espeak"  text))))
   ("Read Region" . (lambda ()
                      (if (use-region-p)
                          (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
                            (start-process "espeak" nil "espeak"  text))
                        (message "No region selected."))))))


(defun olivia/M-x ()
  "Open Olivia's root menu using Ivy."
  (interactive)
  (olivia/menu "Menu:" "root" #'olivia/M-x))



(provide 'olivia-menu)
