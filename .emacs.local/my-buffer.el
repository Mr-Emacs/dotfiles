;;; my-buffer.el

(add-to-list 'display-buffer-alist
             '("\\*\\(compilation\\|Shell Command Output\\|Async Shell Command\\|Messages\\|shell\\|vterm\\)\\*"
               (display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.3)
               (window-parameters . ((no-delete-other-windows . t)))))

(setq display-buffer-base-action nil)

(defun my/force-bottom-window (buffer alist)
  (when (string-match-p
         "\\*\\(compilation\\|Shell Command Output\\|Async Shell Command\\|Messages\\|shell\\|vterm\\)\\*"
         (buffer-name buffer))
    (display-buffer-in-side-window buffer
      (append alist '((side . bottom) (slot . 0) (window-height . 0.3))))))

(defvar my/last-editing-buffer nil
  "Last buffer that was being edited before switching to compilation.")

(defun my/switch-to-compilation-buffer ()
  "Switch between current editing buffer and compilation buffer at bottom."
  (interactive)
  (let* ((windows (window-list nil 'no-minibuf))
         (bottom-window (car (sort windows 
                                   (lambda (w1 w2)
                                     (> (window-pixel-top w1)
                                        (window-pixel-top w2))))))
         (current-window (selected-window)))
    (if (eq current-window bottom-window)
        (progn
          (when my/last-editing-buffer
            (let ((last-window (get-buffer-window my/last-editing-buffer)))
              (if last-window
                  (select-window last-window)
                (switch-to-buffer my/last-editing-buffer)))))
      (progn
        (setq my/last-editing-buffer (current-buffer))
        (select-window bottom-window)
        (when (string-match-p
               "\\*\\(compilation\\|Shell Command Output\\|Async Shell Command\\|Messages\\|shell\\|vterm\\)\\*"
               (buffer-name (current-buffer)))
          nil)))))

(global-set-key (kbd "C-'") 'my/switch-to-compilation-buffer)

(provide 'my-buffer)
;;; my-buffer.el ends here
