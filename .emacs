(package-initialize)
(setq custom-file"~/.emacs.custom.el")
(setq package-install-upgrade-built-in t)
(setq default-frame-alist '((undecorated . nil) (fullscreen . nil)))


(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(load custom-file)
(add-to-list 'default-frame-alist `(font . "Iosevka-20"))

(rc/require-theme 'gruber-darker)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(ido-mode 1)
(ido-everywhere 1)
(column-number-mode 1)
(global-display-line-numbers-mode)
(electric-pair-mode 1)

;; Save temporary files out of my current working directory please
(setq backup-directory-alist '(("." . "~/.emacs.d/tmp-files/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))

(require 'dired-win)
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.[b]\\'" . simpc-mode))

(require 'vlog-mode)

; PACKAGES
(rc/require 'dash)
(rc/require 'dash-functional)

(rc/require 'smex)
(global-set-key (kbd "M-x") 'smex)
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

(rc/require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'move-text-up)
            (local-set-key (kbd "M-n") 'move-text-down)))

(require 'my-man)
(global-set-key (kbd "C-c m") #'my/fullscreen-man)

(rc/require 'rust-mode)
(rc/require 'lua-mode)
(rc/require 'go-mode)

(rc/require 'markdown-preview-mode)
(rc/require 'company)
(require 'company)
(global-company-mode)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (interactive)
            (company-mode 0)))
