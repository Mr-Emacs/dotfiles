;; -*- lexical-binding: t; -*-
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(package-initialize)
(setq custom-file"~/.emacs.custom.el")
(load custom-file)
(setq package-install-upgrade-built-in t)
(setq-default word-wrap t)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq-default indent-tabs-mode nil)
(setq scroll-step 3)
(setq todo-file "todo.txt")
(setq log-file "log.txt")
(setq note-file "note.txt")
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
  (whitespace-mode)
  (visual-line-mode)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun my/split-window-on-startup ()
  (when (one-window-p)
    (split-window-right)))
(add-hook 'emacs-startup-hook #'my/split-window-on-startup)

(add-hook 'simpc-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'storth-mode-hook 'rc/set-up-whitespace-handling)
(add-hook 'emacs-lisp-mode 'rc/set-up-whitespace-handling)
(add-hook 'org-mode-hook 'rc/set-up-whitespace-handling)
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

(defun load-todo ()
  (interactive)
  (find-file todo-file))

(defun load-note ()
  (interactive)
  (find-file note-file))

(setq simpc-asm-preview-intel-syntax t)
(setq simpc-asm-preview-strip-directives t)
(setq simpc-asm-preview-flags '("-O0" "-std=c99"))
(when (eq system-type 'windows-nt)
  (setq simpc-asm-preview-compiler "clang.exe"))

(require 'ssh-connect)
(rc/require 'helm)
(setq fixme-modes '(simpc-mode emacs-lisp-mode rust-mode))

(make-face 'font-lock-fixme-face)
(make-face 'font-lock-note-face)
(modify-face 'font-lock-fixme-face "Red" nil nil t nil t nil nil)
(modify-face 'font-lock-note-face "Dark Green" nil nil t nil t nil nil)

(mapc (lambda (mode)
        (font-lock-add-keywords
         mode
         '(("\\<\\(FIXME\\|TODO\\)" 1 'font-lock-fixme-face t)
           ("\\<\\(NOTE\\)" 1 'font-lock-note-face t))))
      fixme-modes)

(setq global-hl-line-sticky-flag t)
(global-hl-line-mode)
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

(add-to-list 'default-frame-alist '(font . "Liberation Mono-12"))
(set-face-attribute 'default t :font "Liberation Mono-12")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")

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
  (set-foreground-color "burlywood3")
  (set-background-color "#161616")
  (set-cursor-color "#40FF40")
  (set-face-background 'hl-line "midnight blue")
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil) (hl-line-mode -1)))
  (add-hook 'dired-mode-hook (lambda () (setq-local global-hl-line-mode nil) (hl-line-mode -1))))

(require 'simpc-asm-preview)
(add-hook 'simpc-mode-hook #'simpc-asm-preview-mode)
(add-hook 'window-setup-hook 'post-load-stuff t)

(defun setup-msvc ()
  (interactive)
  (when (eq system-type 'windows-nt)
    (let* ((vcvarsall "C:/Program Files/Microsoft Visual Studio/18/Community/VC/Auxiliary/Build/vcvarsall.bat")
           (output (shell-command-to-string
                    (concat "cmd /c \"\"" vcvarsall "\" x64 && set\""))))
      (dolist (line (split-string output "\n"))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let ((var (match-string 1 line))
                (val (match-string 2 line)))
            (setenv var val)
            (when (string= var "PATH")
              (setq exec-path (split-string val ";")))))))))

(defun ediff-setup-windows (buffer-A buffer-B buffer-C control-buffer)
  (ediff-setup-windows-plain buffer-A buffer-B buffer-C control-buffer))

(setq ediff-window-setup-function 'ediff-setup-windows)
(setq ediff-split-window-function 'split-window-horizontally)

(setup-msvc)

(when (eq system-type 'gnu/linux)
  (rc/require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  (when (memq window-system '(mac ns x))
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(when (eq system-type 'windows-nt)
  (setq shell-file-name "powershell")
  (setq shell-command-switch "-Command"))

(require 'ansi-color)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(add-hook 'shell-filter-hook 'ansi-color-compilation-filter)

(rc/require 'org-present)
(require 'org)
(with-eval-after-load 'org
  (setq org-src-lang-modes
        (cons '("c" . simpc)
              (assq-delete-all "c" org-src-lang-modes))))
(setq org-edit-src-content-indentation 0)

(setq org-use-fast-todo-selection t)
(setq org-return-follows-link t)
(setq org-mouse-1-follows-link t)
(define-key org-mode-map [mouse-1] #'org-toggle-checkbox)

(defun insert-current-time ()
  "Insert the current time at point."
  (interactive)
  (insert (format-time-string "%H:%M:%S")))

(global-set-key (kbd "C-c t") 'insert-current-time)

(defun generate-tags-windows ()
  (interactive)
  (shell-command
   "cmd /c \"for /r . %f in (*.c *.h) do etags --append %f\""))
(defun generate-tags-linux ()
  (interactive)
  (shell-command
   "find . -name '*.c' -o -name '*.h' | etags -"))

(defun generate-tags ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (generate-tags-windows)
    (generate-tags-linux))
  (message "TAGS generated in %s" default-directory))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-at-bottom)
               (inhibit-same-window . t)))

(add-to-list 'display-buffer-alist
             '("\\*Async Shell Command\\*"
               (display-buffer-at-bottom)
               (inhibit-same-window . t)))

(defun refresh-path ()
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (let ((path (getenv "PATH")))
      (setq exec-path (split-string path ";"))
      (message "exec-path refreshed from Windows PATH")))
   (t
    (let ((path (replace-regexp-in-string
                 "\n" ""
                 (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
      (setenv "PATH" path)
      (setq exec-path (split-string path ":"))
      (message "exec-path refreshed from shell PATH")))))

(defun insert-simpc-mode()
  (interactive)
  (insert "/* -*- mode: simpc -*- */"))
