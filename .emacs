;; -*- lexical-binding: t; -*-
(package-initialize)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (set-frame-font "Hack-22" t t)))
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq package-install-upgrade-built-in t)
(setq org-html-validation-link nil)
(setq-default word-wrap t)
(setq dired-dwim-target t)
(setq org-agenda-files '("~/dotfiles/agenda.org"))
(setq whitespace-style '(face tabs spaces trailing space-before-tab space-after-tab space-mark tab-mark))
(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path
             (expand-file-name "~/.emacs.local/"))

(load-theme 'dark)
(add-hook 'org-mode-hook #'visual-line-mode)
(global-hl-line-mode 1)
(setq global-hl-line-sticky-flag t)

(dolist (hook '(python-mode-hook
                js-mode-hook
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

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(ido-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(rc/require 'ido-completing-read+)
(ido-ubiquitous-mode)

(column-number-mode 1)

(global-display-line-numbers-mode 1)

(dolist (mode '(eshell-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(electric-pair-mode 1)

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
(rc/require 'zig-mode)

(rc/require 'move-text)
(global-set-key (kbd "M-n") 'move-text-down)
(global-set-key (kbd "M-p") 'move-text-up)
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'move-text-up)
            (local-set-key (kbd "M-n") 'move-text-down)))

(rc/require 'rust-mode)
(rc/require 'auctex-label-numbers)
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and buffer-file-name
               (string-match-p "\\.tex\\'" buffer-file-name))
      (auctex-label-numbers-mode 1))))
;;(rc/require 'markdown-preview-mode)
(rc/require 'company)
(global-company-mode)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(rc/require 'vterm)

(require 'vterm-toggle)
(require 'vterm-buffer)
(require 'url-grabber)
(require 'ssh-connect)
(require 'generate-tags)
(require 'vterm-mux)
(require 'todo-mode)
(require 'cgoogle)

(rc/require 'mmm-mode)
(require 'chc-mode)

(global-set-key (kbd "C-<return>") #'vterm-toggle-new-window)
(global-set-key (kbd "C-x t") #'vterm-toggle-vertical-split)
(global-set-key (kbd "C-c s") #'vterm-switch-buffer-dmenu)

(global-set-key (kbd "C-c C-k") #'vterm-copy-mode)
(global-set-key (kbd "C-c k") #'vterm-copy-mode-done)

(rc/require 'yasnippet)
(yas-global-mode)
(rc/require 'rainbow-mode)
(rc/require 'just-mode)

(setq confirm-kill-processes nil)
(setq confirm-kill-emacs nil)

(add-hook 'kill-emacs-hook
          (lambda () (save-some-buffers t)))

(global-set-key (kbd "C-c g") 'grep)

(require 'man)
(set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
(set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t)
(global-set-key (kbd "C-c m") 'man)

(global-set-key (kbd "C-c C-g") #'cgoogle-search)
(setq initial-scratch-message "")
(add-hook 'vterm-mode-hook
          (lambda ()
            (setq-local global-hl-line-mode nil)
            (hl-line-mode -1)))
