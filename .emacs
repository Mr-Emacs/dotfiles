;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(add-to-list 'default-frame-alist '(font . "Hack Nerd Font 14"))
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
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.local/"))

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

(setq backup-directory-alist '(("." . "~/.emacs.d/tmp-files/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq vterm-term-environment-variable "xterm-256color")

(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
(add-to-list 'auto-mode-alist '("\\.[b]\\'" . simpc-mode))

(require 'vlog-mode)

(rc/require 'haskell-mode)
(rc/require 'eshell-toggle 'eshell-git-prompt)

(eshell-git-prompt-use-theme 'robbyrussell)
(global-set-key (kbd "C-c e") 'eshell-toggle)

(defun eshell-new (name)
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(global-set-key (kbd "C-c C-t") 'eshell-new)

(rc/require 'ivy 'ivy-posframe)
(ivy-mode)

(require 'csode-menu)
(global-set-key (kbd "M-x") 'csode/M-x)

(rc/require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

(require 'seq)
(rc/require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

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

(rc/require 'rust-mode 'cmake-mode)
(rc/require 'yaml-mode)
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
(global-set-key (kbd "C-c C-c") 'compile)

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

(rc/require 'nerd-icons-dired)
(add-hook 'dired-mode-hook
          (lambda ()
            (nerd-icons-dired-mode 1)))

(rc/require 'doom-modeline)
(doom-modeline-mode)

(rc/require 'org-bullets 'org-superstar 'org-present)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (org-superstar-mode 1)))

(defun custom-org-heading-sizes ()
  "Set custom sizes for Org mode headings."
  (set-face-attribute 'org-level-1 nil :height 1.5)
  (set-face-attribute 'org-level-2 nil :height 1.4)
  (set-face-attribute 'org-level-3 nil :height 1.3)
  (set-face-attribute 'org-level-4 nil :height 1.2)
  (set-face-attribute 'org-level-5 nil :height 1.1))

(add-hook 'org-mode-hook 'custom-org-heading-sizes)

(setq org-hide-block-startup t)
(setq org-src-preserve-indentation t)
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook #'org-indent-mode)


(rc/require 'exwm)

(require 'exwm)
(require 'exwm-input)
(require 'exwm-randr)
(require 'exwm-systemtray)

(setq exwm-workspace-number 5)
(setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j
      ?\C-\ ))

;; Monitor mapping
(setq exwm-randr-workspace-monitor-plist '(0 "HDMI-0" 1 "DP-1"))
(exwm-randr-mode)
(exwm-systemtray-mode)

(exwm-input-set-key (kbd "M-x") 'csode/M-x)
(start-process-shell-command
 "xrandr" nil
 "xrandr --output DP-1 --mode 1920x1080 --pos 0x0 --rotate normal \
          --output HDMI-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal")

(setq exwm-input-global-keys
      `(

        ([s-return] . eshell)
        ([s-M-return] . vterm-toggle-new-window)

        ([?\s-r] . exwm-reset)
        ([?\s-q] . kill-emacs)
        ([?\s-w] . exwm-workspace-switch)

        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
))

(exwm-enable)

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
