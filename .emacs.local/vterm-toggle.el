;;; vterm-toggle.el --- Toggle vterm buffer with one key -*- lexical-binding: t; -*-

(require 'vterm)

(defun vterm-toggle ()
  "Toggle vterm buffer in horizontal split for current buffer's directory.
If vterm is visible, bury and close it.
If not, open vterm in the current buffer's directory."
  (interactive)
  (let* ((default-dir (or (and (buffer-file-name) (file-name-directory (buffer-file-name)))
                          default-directory))
         (vterm-buf-name (format "*vterm: %s*" (abbreviate-file-name default-dir)))
         (vterm-buf (get-buffer vterm-buf-name))
         (vterm-visible (get-buffer-window vterm-buf))
         (current-win (selected-window)))
    (if (and vterm-buf vterm-visible)
        ;; Vterm is visible: hide it
        (progn
          (bury-buffer vterm-buf)
          (delete-window vterm-visible)
          (message "VTerm hidden"))
      ;; Vterm not visible: show or create it
      (let ((new-win (split-window-horizontally)))
        (select-window new-win)
        (if vterm-buf
            (switch-to-buffer vterm-buf)
          ;; Create new vterm buffer in the desired directory
          (let ((default-directory default-dir)) ; Set dir for new vterm
            (vterm vterm-buf-name)))))))

(provide 'vterm-toggle)
;;; vterm-toggle.el ends here
