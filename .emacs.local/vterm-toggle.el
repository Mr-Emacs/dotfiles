;;; vterm-toggle.el --- Toggle vterm buffer with one key -*- lexical-binding: t; -*-

(require 'vterm)
(require 'cl-lib)

(defun vterm-toggle--close-all-vterm-windows ()
  "Close all visible vterm windows and bury their buffers."
  (dolist (window (window-list))
    (let ((buffer (window-buffer window)))
      (when (and buffer 
                 (string-match-p "\\*vterm:" (buffer-name buffer))
                 (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
        (bury-buffer buffer)
        (when (and (window-live-p window)
                   (not (eq window (selected-window)))
                   (> (length (window-list)) 1))
          (delete-window window))))))

(defun vterm-toggle--any-vterm-visible-p ()
  "Return t if any vterm window is currently visible."
  (cl-some
   (lambda (window)
     (let ((buffer (window-buffer window)))
       (and buffer
            (string-match-p "\\*vterm:" (buffer-name buffer))
            (with-current-buffer buffer (derived-mode-p 'vterm-mode)))))
   (window-list)))

;;;###autoload
(defun vterm-toggle-split ()
  "Toggle vterm buffer in horizontal split for current buffer's directory.
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in the current buffer's directory."
  (interactive)
  (let* ((default-dir (or (and (buffer-file-name) 
                               (file-name-directory (buffer-file-name)))
                          default-directory))
         (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
         (vterm-buf (get-buffer vterm-buf-name)))
    
    (if (vterm-toggle--any-vterm-visible-p)
        (progn
          (vterm-toggle--close-all-vterm-windows)
          (message "Closed existing vterm windows"))
      ;; Open vterm in horizontal split
      (let ((new-win (split-window-below))) ; Changed to vertical split (new window below)
        (select-window new-win)
        (if (and vterm-buf (buffer-live-p vterm-buf))
            (progn
              (switch-to-buffer vterm-buf)
              ;; Change to the correct directory if needed
              (when (not (string= default-directory default-dir))
                (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
                (vterm-send-return)))
          (let ((default-directory default-dir))
            (vterm vterm-buf-name)))))))

;;;###autoload
(defun vterm-toggle-fullscreen ()
  "Toggle fullscreen vterm buffer.
If any vterm is visible, close all and exit.
If none visible, open vterm fullscreen in the current buffer's directory."
  (interactive)
  (if (vterm-toggle--any-vterm-visible-p)
      (progn
        (vterm-toggle--close-all-vterm-windows)
        (message "Closed existing vterm windows"))
    ;; Open fullscreen vterm
    (let* ((default-dir (or (and (buffer-file-name)
                                 (file-name-directory (buffer-file-name)))
                            default-directory))
           (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
           (vterm-buf (get-buffer vterm-buf-name)))
      
      (delete-other-windows)
      (if (and vterm-buf (buffer-live-p vterm-buf))
          (progn
            (switch-to-buffer vterm-buf)
            ;; Change to the correct directory if needed
            (when (not (string= default-directory default-dir))
              (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
              (vterm-send-return)))
        (let ((default-directory default-dir))
          (vterm vterm-buf-name))))))

;;;###autoload  
(defun vterm-toggle-new-window ()
  "Toggle vterm buffer in a new window (frame).
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in a new window/frame."
  (interactive)
  (if (vterm-toggle--any-vterm-visible-p)
      (progn
        (vterm-toggle--close-all-vterm-windows)
        (message "Closed existing vterm windows"))
    ;; Open vterm in new frame
    (let* ((default-dir (or (and (buffer-file-name)
                                 (file-name-directory (buffer-file-name)))
                            default-directory))
           (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
           (vterm-buf (get-buffer vterm-buf-name))
           (new-frame (make-frame)))
      
      (select-frame new-frame)
      (if (and vterm-buf (buffer-live-p vterm-buf))
          (progn
            (switch-to-buffer vterm-buf)
            ;; Change to the correct directory if needed
            (when (not (string= default-directory default-dir))
              (vterm-send-string (format "cd %s" (shell-quote-argument default-dir)))
              (vterm-send-return)))
        (let ((default-directory default-dir))
          (vterm vterm-buf-name))))))

(provide 'vterm-toggle)
;;; vterm-toggle.el ends here
