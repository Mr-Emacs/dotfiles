 ;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq package-install-upgrade-built-in t)
(setq-default word-wrap t)
(setq compilation-scroll-output t)
(load-file "~/.emacs.rc/rc.el")
(load "~/.emacs.rc/misc-rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.local/"))
(setq default-directory
      (cond
       ((eq system-type 'gnu/linux) "~/Programming/")
       ((eq system-type 'windows-nt)
        "C:/Programming/")
       (t "~/")))

(defun rc/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Maple Mono NL-12")
   ((eq system-type 'gnu/linux) "Ubuntu mono-14")))

(add-to-list 'default-frame-alist `(font . ,(rc/get-default-font)))

(defun my/split-window-on-startup ()
  (when (one-window-p)
    (split-window-right)))

(add-hook 'emacs-startup-hook #'my/split-window-on-startup)

(when (eq system-type 'gnu/linux)
  (require 'ssh-connect)
  (rc/require 'helm)
  (global-set-key (kbd "C-c g") 'grep))

(load-theme 'wheatgrass)
(require 'todo-mode)

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
(global-set-key (kbd "C-c C-c") 'compile)

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
(rc/require 'mmm-mode)
(require 'chc-mode)

(defun my/display-buffer-right-only (buffer alist)
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
