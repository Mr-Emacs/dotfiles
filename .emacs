;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq whitespace-style '(face tabs spaces trailing space-before-tab space-after-tab space-mark tab-mark))
(setq package-install-upgrade-built-in t)
(setq org-html-validation-link nil)
(setq-default word-wrap t)
(setq dired-dwim-target t)
(setq org-agenda-files '("~/dotfiles/agenda.org"))
(setq eglot-autoloads nil)
(setq compilation-scroll-output t)
(setq eglot-ignored-server-capabilities '(:documentHighlight))
(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.local/"))
(setq default-directory
      (cond
       ((eq system-type 'gnu/linux) "~/projects/")
       ((eq system-type 'windows-nt)
        "C:/Users/tadih/Documents/Programming/")
       (t "~/")))

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Maple Mono NL-12")
   ((eq system-type 'gnu/linux) "Ubuntu mono-20")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(defun my/split-window-on-startup ()
  "Split window vertically on startup."
  (when (one-window-p)
    (split-window-right)))

(add-hook 'emacs-startup-hook #'my/split-window-on-startup)

(defun my-attach-whitespace-mode-hooks ()
  (when (or (string= (car custom-enabled-themes) "cmp-darker")
            (string= (car custom-enabled-themes) "gruber-darker"))
    (dolist (hook '(python-mode-hook
                    js-mode-hook
                    asm-mode-hook
                    emacs-lisp-mode-hook
                    simpc-mode-hook
                    c-mode-hook
                    java-mode-hook
                    ruby-mode-hook
                    go-mode-hook
                    rust-mode-hook
                    sh-mode-hook
                    yaml-mode-hook
                    markdown-mode-hook
                    aoxim-mode-hook
                    d-mode-hook
                    org-mode-hook))
      (add-hook hook 'whitespace-mode))))

(when (eq system-type 'gnu/linux)
  (rc/require 'vterm)
  (require 'vterm-mux)
  (require 'vterm-toggle)
  (require 'ssh-connect)
  (require 'generate-tags)
  (require 'cgoogle)
  (rc/require 'helm)
  (global-set-key (kbd "C-c g") 'grep)
  (global-set-key (kbd "C-c C-g") #'cgoogle-search))

(load-theme 'wheatgrass)
(require 'todo-mode)

(rc/require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)

(rc/require 'ido-completing-read+)
(ido-ubiquitous-mode)

(my-attach-whitespace-mode-hooks)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq global-hl-line-sticky-flag t)
(global-hl-line-mode -1)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(which-key-mode 0)
(global-display-line-numbers-mode -1)

(column-number-mode)
(electric-pair-mode)

(setq backup-directory-alist '(("." . "~/.emacs.d/tmp-files/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))
(setq lock-file-name-transforms `((".*" "~/.emacs.d/tmp-files/" t)))

(require 'aoxim-mode)

(rc/require 'eshell-toggle 'eshell-git-prompt)

(eshell-git-prompt-use-theme 'robbyrussell)
(global-set-key (kbd "C-c e") 'eshell-toggle)

(defun eshell-new (name)
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(global-set-key (kbd "C-c C-t") 'eshell-new)

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

(rc/require 'rust-mode 'd-mode 'cmake-mode 'yaml-mode)
(rc/require 'company)
(global-company-mode)

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'fasm-mode)
(require 'highlight-todo-mode)

(rc/require 'mmm-mode 'meson-mode)
(require 'chc-mode)
(require 'umka-mode)

(rc/require 'yasnippet)
(yas-global-mode)

(defun my-disable-hl-line-mode ()
  "Disable hl-line-mode and display-line-numbers-mode."
  (hl-line-mode -1)
  (display-line-numbers-mode -1))

(add-hook 'dired-mode-hook 'my-disable-hl-line-mode)
(add-hook 'eshell-mode-hook 'my-disable-hl-line-mode)
(add-hook 'compilation-mode-hook 'my-disable-hl-line-mode)

(defun my/display-buffer-right-only (buffer alist)
  "Display BUFFER in the rightmost window, never the left one.
If no right window exists, split vertically."
  (let ((right-window
         (or (window-in-direction 'right)
             (when (one-window-p)
               (split-window-right)))))
    (when right-window
      (set-window-buffer right-window buffer)
      right-window)))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               my/display-buffer-right-only))

(add-to-list 'display-buffer-alist
             '("\\*Shell Command Output\\*"
               my/display-buffer-right-only))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               my/display-buffer-right-only))

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
