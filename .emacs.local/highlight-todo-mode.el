;; highlight-todo-mode.el

(defface my-todo-face
  '((t (:foreground "yellow" :weight bold)))
  "Face for TODO comments.")

(defface my-fixme-face
  '((t (:foreground "red" :weight bold)))
  "Face for FIXME comments.")

(defface my-note-face
  '((t (:foreground "blue" :weight bold)))
  "Face for NOTE comments.")

(defface my-hack-face
  '((t (:foreground "green" :weight bold)))
  "Face for HACK comments.")

(define-minor-mode highlight-todo-mode
  "Minor mode to highlight TODO, FIXME, NOTE, and HACK comments."
  :lighter " HighlightTODO"
  (if highlight-todo-mode
      (font-lock-add-keywords
       nil
       '(("\\<\\(TODO\\):" 0 'my-todo-face t)
         ("\\<\\(FIXME\\):" 0 'my-fixme-face t)
         ("\\<\\(NOTE\\):" 0 'my-note-face t)
         ("\\<\\(HACK\\):" 0 'my-hack-face t)))
    (font-lock-remove-keywords
     nil
     '(("\\<\\(TODO\\):" 0 'my-todo-face t)
       ("\\<\\(FIXME\\):" 0 'my-fixme-face t)
       ("\\<\\(NOTE\\):" 0 'my-note-face t)
       ("\\<\\(HACK\\):" 0 'my-hack-face t)))
    (font-lock-refresh-defaults)))

(add-hook 'prog-mode-hook 'highlight-todo-mode)

(provide 'highlight-todo-mode)
;;; highlight-todo-mode.el ends here
