;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq package-install-upgrade-built-in t)
(setq-default word-wrap t)
(setq compilation-scroll-output t)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq eglot-extend-to-xref t)
(setq scroll-step 3)
(load-file "~/.emacs.rc/rc.el")
(add-to-list 'load-path "~/.emacs.local/")
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.local/"))
(setq default-directory
      (cond
       ((eq system-type 'gnu/linux) "~/Programming/")
       ((eq system-type 'windows-nt)
        "C:/Programming/")
       (t "~/")))

(defun rc/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'storth-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'rust-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'markdown-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'haskell-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'python-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'fasm-mode-hook 'rc/set-up-whitespace-handling)
(custom-set-faces
 '(whitespace-space ((t (:foreground "#444444" :background unspecified)))))

(rc/require 'smex 'ido-completing-read+)
(require 'ido-completing-read+)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x M-x") 'execute-extended-command)

(defun my/duplicate-line ()
  "Duplicate the current line below, moving cursor to the new duplicated line."
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (let ((line (thing-at-point 'line t)))
        (end-of-line)
        (newline)
        (insert (string-remove-suffix "\n" line))))
    (forward-line 1)
    (move-to-column col)))

(global-set-key (kbd "C-,") 'my/duplicate-line)
(load-theme 'wheatgrass)

(setq simpc-asm-preview-intel-syntax t)
(setq simpc-asm-preview-strip-directives t)
(setq simpc-asm-preview-flags '("-O0" "-std=c99"))
(when (eq system-type 'windows-nt)
  (setq simpc-asm-preview-compiler "clang.exe"))

(defun my/split-window-on-startup ()
  (when (one-window-p)
    (split-window-right)))
(add-hook 'emacs-startup-hook #'my/split-window-on-startup)

(require 'project-notes)

(when (eq system-type 'gnu/linux)
  (require 'ssh-connect)
  (rc/require 'helm 'vterm)
  (require 'vterm-buffer)
  (require 'vterm-mux)
  (require 'vterm-toggle)
  ;; (rc/require-theme 'gruber-darker)
  (global-set-key (kbd "C-c g") 'grep))

(setq fixme-modes '(simpc-mode emacs-lisp-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(setq global-hl-line-sticky-flag t)
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

(rc/require 'rust-mode 'company 'markdown-mode)
(global-company-mode)

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'aoxim-mode)
(require 'fasm-mode)
(require 'simpc-mode)
(require 'bufo-mode)

(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(defun astyle-buffer ()
  (interactive)
  (let ((saved-line-number (line-number-at-pos)))
    (shell-command-on-region
     (point-min)
     (point-max)
     "astyle --style=kr"
     nil
     t)
    (goto-line saved-line-number)))

(add-to-list 'default-frame-alist '(font . "Iosevka-11.5"))
(set-face-attribute 'default t :font "Iosevka-11.5")

;;; dired
(require 'dired-x)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")
(setq dired-mouse-drag-files t)
(setq dired-recursive-deletes 'always)

(defun post-load-stuff ()
  (interactive)
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil) (hl-line-mode -1)))
  (add-hook 'dired-mode-hook (lambda () (setq-local global-hl-line-mode nil) (hl-line-mode -1))))

(require 'simpc-asm-preview)
(add-hook 'simpc-mode-hook #'simpc-asm-preview-mode)
(add-hook 'window-setup-hook 'post-load-stuff t)

(defun setup-msvc ()
  (when (eq system-type 'windows-nt)
    (let* ((vcvarsall "C:/Program Files (x86)/Microsoft Visual Studio/2019/Community/VC/Auxiliary/Build/vcvarsall.bat")
           (output (shell-command-to-string
                    (concat "cmd /c \"\"" vcvarsall "\" x64 && set\""))))
      (dolist (line (split-string output "\n"))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let ((var (match-string 1 line))
                (val (match-string 2 line)))
            (setenv var val)
            (when (string= var "PATH")
              (setq exec-path (split-string val ";")))))))))

(setup-msvc)

(when (eq system-type 'gnu/linux)
(rc/require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)))
