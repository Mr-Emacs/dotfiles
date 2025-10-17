;;; vterm-mux.el --- Terminal multiplexer for vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; A tmux-like multiplexer for vterm in Emacs
;; Provides multiple terminal sessions, window splitting, and easy navigation

;;; Code:

(require 'vterm)

(defgroup vterm-mux nil
  "Terminal multiplexer for vterm."
  :group 'vterm)

(defcustom vterm-mux-prefix-key "C-z"
  "Prefix key for vterm-mux commands (like tmux's C-b)."
  :type 'string
  :group 'vterm-mux)

(defvar vterm-mux-sessions '()
  "List of vterm sessions.")

(defvar vterm-mux-current-session 0
  "Index of current vterm session.")

(defvar vterm-mux-command-map
  (let ((map (make-sparse-keymap)))
    ;; Session management
    (define-key map (kbd "c") 'vterm-mux-create-session)
    (define-key map (kbd "n") 'vterm-mux-next-session)
    (define-key map (kbd "p") 'vterm-mux-prev-session)
    (define-key map (kbd "k") 'vterm-mux-kill-session)
    (define-key map (kbd "K") 'vterm-mux-kill-session-ido)
    (define-key map (kbd "l") 'vterm-mux-list-sessions)
    (define-key map (kbd "0") 'vterm-mux-select-session-0)
    (define-key map (kbd "1") 'vterm-mux-select-session-1)
    (define-key map (kbd "2") 'vterm-mux-select-session-2)
    (define-key map (kbd "3") 'vterm-mux-select-session-3)
    (define-key map (kbd "4") 'vterm-mux-select-session-4)
    (define-key map (kbd "5") 'vterm-mux-select-session-5)
    (define-key map (kbd "6") 'vterm-mux-select-session-6)
    (define-key map (kbd "7") 'vterm-mux-select-session-7)
    (define-key map (kbd "8") 'vterm-mux-select-session-8)
    (define-key map (kbd "9") 'vterm-mux-select-session-9)
    
    ;; Window splitting
    (define-key map (kbd "s") 'vterm-mux-split-horizontal)
    (define-key map (kbd "-") 'vterm-mux-split-horizontal)
    (define-key map (kbd "v") 'vterm-mux-split-vertical)
    (define-key map (kbd "|") 'vterm-mux-split-vertical)
    (define-key map (kbd "o") 'other-window)
    (define-key map (kbd "x") 'delete-window)
    
    ;; Navigation
    (define-key map (kbd "<up>") 'windmove-up)
    (define-key map (kbd "<down>") 'windmove-down)
    (define-key map (kbd "<left>") 'windmove-left)
    (define-key map (kbd "<right>") 'windmove-right)
    
    ;; Misc
    (define-key map (kbd "d") 'vterm-mux-detach)
    (define-key map (kbd "?") 'vterm-mux-help)
    map)
  "Keymap for vterm-mux commands after prefix.")

(defvar vterm-mux-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd vterm-mux-prefix-key) vterm-mux-command-map)
    map)
  "Keymap for vterm-mux mode.")

;;;###autoload
(define-minor-mode vterm-mux-mode
  "Minor mode for vterm multiplexer functionality."
  :lighter " VMux"
  :keymap vterm-mux-mode-map)

(defun vterm-mux-create-session (&optional name)
  "Create a new vterm session with optional NAME."
  (interactive)
  (let* ((session-num (length vterm-mux-sessions))
         (session-name (or name (format "vterm-%d" session-num)))
         (buf (generate-new-buffer session-name)))
    (with-current-buffer buf
      (vterm-mode)
      (vterm-mux-mode 1))
    (push buf vterm-mux-sessions)
    (switch-to-buffer buf)
    (setq vterm-mux-current-session 0)
    (message "Created session: %s" session-name)))

(defun vterm-mux-next-session ()
  "Switch to next vterm session."
  (interactive)
  (when vterm-mux-sessions
    (setq vterm-mux-current-session
          (mod (1+ vterm-mux-current-session) (length vterm-mux-sessions)))
    (switch-to-buffer (nth vterm-mux-current-session vterm-mux-sessions))
    (message "Session %d/%d" 
             (1+ vterm-mux-current-session)
             (length vterm-mux-sessions))))

(defun vterm-mux-prev-session ()
  "Switch to previous vterm session."
  (interactive)
  (when vterm-mux-sessions
    (setq vterm-mux-current-session
          (mod (1- vterm-mux-current-session) (length vterm-mux-sessions)))
    (switch-to-buffer (nth vterm-mux-current-session vterm-mux-sessions))
    (message "Session %d/%d"
             (1+ vterm-mux-current-session)
             (length vterm-mux-sessions))))

(defun vterm-mux-select-session (n)
  "Switch to session number N."
  (interactive "nSession number: ")
  (if (and (>= n 0) (< n (length vterm-mux-sessions)))
      (progn
        (setq vterm-mux-current-session n)
        (switch-to-buffer (nth n vterm-mux-sessions))
        (message "Switched to session %d" n))
    (message "Invalid session number")))

;; Define individual session selection functions for direct keybinding
(dotimes (i 10)
  (eval `(defun ,(intern (format "vterm-mux-select-session-%d" i)) ()
           ,(format "Switch to session %d." i)
           (interactive)
           (vterm-mux-select-session ,i))))

(defun vterm-mux-kill-session ()
  "Kill current vterm session."
  (interactive)
  (when vterm-mux-sessions
    (let ((buf (nth vterm-mux-current-session vterm-mux-sessions)))
      (setq vterm-mux-sessions (delq buf vterm-mux-sessions))
      (kill-buffer buf)
      (when vterm-mux-sessions
        (setq vterm-mux-current-session
              (min vterm-mux-current-session (1- (length vterm-mux-sessions))))
        (switch-to-buffer (nth vterm-mux-current-session vterm-mux-sessions)))
      (message "Session killed. %d remaining" (length vterm-mux-sessions)))))

(defun vterm-mux-kill-session-ido ()
  "Kill sessions using ido selection, with option to kill all."
  (interactive)
  (if (not vterm-mux-sessions)
      (message "No active sessions")
    (let* ((session-names (mapcar #'buffer-name vterm-mux-sessions))
           (all-option "** KILL ALL SESSIONS **")
           (choices (cons all-option session-names))
           (selected (ido-completing-read "Kill session: " choices nil t)))
      (cond
       ((string= selected all-option)
        (when (yes-or-no-p (format "Kill all %d sessions? " (length vterm-mux-sessions)))
          (dolist (buf vterm-mux-sessions)
            (kill-buffer buf))
          (setq vterm-mux-sessions '())
          (setq vterm-mux-current-session 0)
          (message "All sessions killed")))
       (t
        (let ((buf (get-buffer selected)))
          (when buf
            (setq vterm-mux-sessions (delq buf vterm-mux-sessions))
            (kill-buffer buf)
            (when vterm-mux-sessions
              (setq vterm-mux-current-session
                    (min vterm-mux-current-session (1- (length vterm-mux-sessions))))
              (switch-to-buffer (nth vterm-mux-current-session vterm-mux-sessions)))
            (message "Session '%s' killed. %d remaining" selected (length vterm-mux-sessions)))))))))

(defun vterm-mux-list-sessions ()
  "List all vterm sessions."
  (interactive)
  (if vterm-mux-sessions
      (let ((session-list
             (mapconcat
              (lambda (buf)
                (format "%s%s"
                        (if (eq buf (current-buffer)) "* " "  ")
                        (buffer-name buf)))
              vterm-mux-sessions
              "\n")))
        (message "Sessions:\n%s" session-list))
    (message "No active sessions")))

(defun vterm-mux-split-horizontal ()
  "Split window horizontally and create new vterm."
  (interactive)
  (split-window-below)
  (other-window 1)
  (vterm-mux-create-session))

(defun vterm-mux-split-vertical ()
  "Split window vertically and create new vterm."
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm-mux-create-session))

(defun vterm-mux-detach ()
  "Detach from vterm-mux (bury all vterm buffers)."
  (interactive)
  (dolist (buf vterm-mux-sessions)
    (bury-buffer buf))
  (message "Detached from vterm-mux"))

(defun vterm-mux-help ()
  "Show vterm-mux keybindings."
  (interactive)
  (message "vterm-mux keys (prefix: %s):
  c - new session    n/p - next/prev session
  k - kill current   K - kill with ido (+ kill all)
  l - list sessions  0-9 - goto session
  s/- - split horiz  v/| - split vertical
  o - other window   x - close window
  arrow keys - nav   d - detach  ? - help"))

;;;###autoload
(defun vterm-mux ()
  "Start vterm-mux with initial session."
  (interactive)
  (setq vterm-mux-sessions '())
  (setq vterm-mux-current-session 0)
  (vterm-mux-create-session)
  (vterm-mux-help))

(provide 'vterm-mux)
;;; vterm-mux.el ends here
