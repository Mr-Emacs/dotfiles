;; -*- lexical-binding: t; -*
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(add-to-list 'default-frame-alist '(font . "Ubuntu mono 18"))
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq whitespace-style '(face tabs spaces trailing space-before-tab space-after-tab space-mark tab-mark))
(setq package-install-upgrade-built-in t)
(setq org-html-validation-link nil)
(setq-default word-wrap t)
(setq dired-dwim-target t)
(setq org-agenda-files '("~/dotfiles/agenda.org"))
(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.emacs.local/"))

(dolist (hook '(python-mode-hook
                js-mode-hook
                asm-mode-hook
                emacs-lisp-mode-hook
                simpc-mode-hook
                java-mode-hook
                ruby-mode-hook
                go-mode-hook
                rust-mode-hook
                sh-mode-hook
                yaml-mode-hook
                markdown-mode-hook
                org-mode-hook))
  (add-hook hook 'whitespace-mode))

(load-theme 'dark)
(add-hook 'org-mode-hook #'visual-line-mode)
(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag t)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(which-key-mode 0)

(column-number-mode 1)

(electric-pair-mode 1)
(global-display-line-numbers-mode 0)
(setq shell-command-switch "-ic")
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp-files/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq vterm-term-environment-variable "xterm-256color")

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.[b]\\'" . simpc-mode))

(require 'vlog-mode)

; PACKAGES
(rc/require 'haskell-mode)

(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(rc/require 'ido-completing-read+)
(ido-ubiquitous-mode)

(rc/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(rc/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(require 'seq)
(rc/require 'magit)
(setq magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1)

(rc/require 'transient)
(rc/require 'markdown-mode)
(rc/require 'lua-mode)

(rc/require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'move-text-up)
            (local-set-key (kbd "M-n") 'move-text-down)))

(rc/require 'rust-mode)

(rc/require 'company)
(global-company-mode)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(rc/require 'vterm)

(require 'vterm-toggle)
(require 'vterm-buffer)
(global-set-key (kbd "C-<return>") #'vterm-toggle-new-window)
(global-set-key (kbd "C-x t") #'vterm-toggle-vertical-split)
(global-set-key (kbd "C-c s") #'vterm-switch-buffer-dmenu)

(global-set-key (kbd "C-c C-k") #'vterm-copy-mode)
(global-set-key (kbd "C-c k") #'vterm-copy-mode-done)

(require 'url-grabber)
(require 'ssh-connect)
(require 'generate-tags)
(require 'vterm-mux)
(require 'todo-mode)
(require 'cgoogle)

(rc/require 'mmm-mode)
(require 'chc-mode)

(rc/require 'yasnippet)
(yas-global-mode)
(rc/require 'just-mode)

(global-set-key (kbd "C-c g") 'grep)

(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
(global-set-key (kbd "C-c m") 'man)

(global-set-key (kbd "C-c C-g") #'cgoogle-search)
(setq initial-scratch-message "")
(defun my-disable-hl-line-mode ()
  "Disable hl-line-mode and display-line-numbers-mode."
  (hl-line-mode -1)
  (display-line-numbers-mode -1))

(add-hook 'vterm-mode-hook 'my-disable-hl-line-mode)
(add-hook 'dired-mode-hook 'my-disable-hl-line-mode)

;; Email
(require 'mu4e)
(setq mu4e-user-mail-address-alist
      `(("tadihailukebe@gmail.com" . ,(concat "tadihailukebe@gmail.com"))))

(setq mu4e-change-filenames-when-moving t)

(setq mu4e-update-interval (* 10 60))
(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-maildir "~/Mail")

(setq mu4e-drafts-folder "/[Gmail]/Drafts")
(setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
(setq mu4e-refile-folder "/[Gmail]/All Mail")
(setq mu4e-trash-folder  "/[Gmail]/Trash")

(setq mu4e-maildir-shortcuts
      '(("/Inbox"             . ?i)
        ("/[Gmail]/Sent Mail" . ?s)
        ("/[Gmail]/Trash"     . ?t)
        ("/[Gmail]/Drafts"    . ?d)
        ("/[Gmail]/All Mail"  . ?a)))

(global-set-key (kbd "C-c C-m") 'mu4e)

;; Reload emacs
(defun reload-emacs-config ()
  "Reload Emacs configuration from ~/.emacs."
  (interactive)
  (let ((config-file "~/.emacs"))
    (if (file-exists-p config-file)
        (progn
          (load-file config-file)
          (message "Emacs configuration reloaded."))
      (message "Configuration file not found: %s" config-file))))

(global-set-key (kbd "C-c r") #'reload-emacs-config)
