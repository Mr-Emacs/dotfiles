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
(setq eglot-ignored-server-capabilities '(:documentHighlight))
(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.local/"))
(set-face-attribute 'default nil :font "Ubuntu mono-20")

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
                    ccalc-mode-hook
                    d-mode-hook
                    org-mode-hook))
      (add-hook hook 'whitespace-mode))))

(rc/require-theme 'gruber-darker)
(rc/require 'vterm)
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

(require 'vlog-mode)
(require 'vterm-mux)
(require 'vterm-toggle)
(require 'simpc-mode)
(require 'ccalc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
;; (add-to-list 'auto-mode-alist '("\\.[b]\\'" . simpc-mode))

(rc/require 'eshell-toggle 'eshell-git-prompt)

(eshell-git-prompt-use-theme 'robbyrussell)
(global-set-key (kbd "C-c e") 'eshell-toggle)

(defun eshell-new (name)
  (interactive "sName: ")
  (setq name (concat "$" name))
  (eshell)
  (rename-buffer name))

(global-set-key (kbd "C-c C-t") 'eshell-new)

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

(require 'ssh-connect)
(require 'generate-tags)
(require 'todo-mode)
(require 'cgoogle)
(require 'fasm-mode)
(require 'highlight-todo-mode)

(rc/require 'mmm-mode 'meson-mode)
(require 'chc-mode)
(require 'umka-mode)

(rc/require 'yasnippet)
(yas-global-mode)
(rc/require 'just-mode)

(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c C-c") 'compile)

(require 'woman)
(global-set-key (kbd "C-c m") 'woman)

(global-set-key (kbd "C-c C-g") #'cgoogle-search)
(setq initial-scratch-message "")

(defun my-disable-hl-line-mode ()
  "Disable hl-line-mode and display-line-numbers-mode."
  (hl-line-mode -1)
  (display-line-numbers-mode -1))

(add-hook 'dired-mode-hook 'my-disable-hl-line-mode)
(add-hook 'eshell-mode-hook 'my-disable-hl-line-mode)
(add-hook 'compilation-mode-hook 'my-disable-hl-line-mode)

(rc/require 'helm)

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
