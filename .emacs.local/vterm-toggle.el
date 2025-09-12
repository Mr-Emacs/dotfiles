;;; vterm-toggle.el --- Toggle vterm buffer with one key -*- lexical-binding: t; -*-

(require 'vterm)

(defun vterm-toggle ()
  "Toggle vterm buffer in horizontal split.
If vterm is visible, bury and close it.
If no visible vterm, show existing buried or create new."
  (interactive)
  (let* ((vterm-bufs (seq-filter
                      (lambda (buf)
                        (with-current-buffer buf
                          (eq major-mode 'vterm-mode)))
                      (buffer-list)))
         (vterm-buf (car vterm-bufs))
         (current-win (selected-window))
         (current-buf (window-buffer current-win)))
    (if (eq major-mode 'vterm-mode)
        ;; If currently in vterm buffer, bury it and delete window
        (progn
          (bury-buffer)
          (delete-window)
          (message "VTerm hidden"))
      ;; Else: show vterm
      (if (and vterm-buf (get-buffer-window vterm-buf))
          ;; If vterm buffer visible somewhere else, just switch to it
          (select-window (get-buffer-window vterm-buf))
        ;; Else: split and show vterm buffer or create new
        (let ((new-win (split-window-horizontally)))
          (select-window new-win)
          (if vterm-buf
              (switch-to-buffer vterm-buf)
            (vterm)))))))

(provide 'vterm-toggle)
;;; vterm-toggle.el ends here
