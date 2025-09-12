;;; vterm-toggle.el --- Toggle vterm buffer with one key -*- lexical-binding: t; -*-

(require 'vterm)

(defun vterm-toggle--close-all-vterm-windows ()
  "Close all visible vterm windows and bury their buffers."
  (dolist (window (window-list))
    (let ((buffer (window-buffer window)))
      (when (and buffer 
                 (string-match-p "\\*vterm:" (buffer-name buffer))
                 (with-current-buffer buffer (derived-mode-p 'vterm-mode)))
        (bury-buffer buffer)
        (when (window-live-p window)
          (delete-window window))))))

;;;###autoload
(defun vterm-toggle ()
  "Toggle vterm buffer in horizontal split for current buffer's directory.
If any vterm is visible, close all vterm windows.
If none are visible, open vterm in the current buffer's directory."
  (interactive)
  (let* ((default-dir (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                          default-directory))
         (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
         (vterm-buf (get-buffer vterm-buf-name))
         (any-vterm-visible (cl-some
                             (lambda (window)
                               (let ((buffer (window-buffer window)))
                                 (and buffer
                                      (string-match-p "\\*vterm:" (buffer-name buffer))
                                      (with-current-buffer buffer (derived-mode-p 'vterm-mode)))))
                             (window-list))))

    (if any-vterm-visible
        (progn
          (vterm-toggle--close-all-vterm-windows)
          (message "Closed existing vterm windows"))
      ;; Else, open vterm
      (let ((new-win (split-window-horizontally)))
        (select-window new-win)
        (if vterm-buf
            (switch-to-buffer vterm-buf)
          (let ((default-directory default-dir))
            (vterm vterm-buf-name)))))))

(provide 'vterm-toggle)
;;; vterm-toggle.el ends here
