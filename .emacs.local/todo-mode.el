;;; todo-mode.el --- todo major mode for org mode

;; Author: xsoder
;; URL: https://github.com/xsoder/dotfiles/.emacs.local/todo-mode.el
;; Version: 0.1.1

;;MIT No Attribution
;;
;;Copyright 2025 xsoder
;;
;;Permission is hereby granted, free of charge, to any person obtaining a copy of this
;;software and associated documentation files (the "Software"), to deal in the Software
;;without restriction, including without limitation the rights to use, copy, modify,
;;merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
;;permit persons to whom the Software is furnished to do so.
;;
;;THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
;;INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
;;PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defun insert-org-checkbox ()
  "Insert a new checkbox item '- [ ]' at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (insert "- [ ] "))

(defun insert-org-todo-with-checkboxes ()
  "Create a new Org TODO heading with an empty checkbox list, interactively."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in Org mode"))
  (org-insert-heading)
  (org-do-demote)
  (org-todo "TODO")
  (insert " ")
  (let ((title (read-string "TODO Title: ")))
    (insert title))
  (insert "\n  - [ ] "))

(defun toggle-org-todo-or-checkbox ()
  "Toggle the current Org item.
If at a heading, toggle TODO ↔ DONE.
If at a checkbox, toggle [ ] ↔ [X].
Otherwise, show a message."
  (interactive)
  (cond
   ((org-at-item-checkbox-p)
    (org-toggle-checkbox))
   ((org-at-heading-p)
    (let ((state (org-get-todo-state)))
      (cond
       ((equal state "TODO") (org-todo "DONE"))
       ((equal state "DONE") (org-todo "TODO"))
       (t (org-todo "TODO")))))
   (t (message "Not on a TODO or checkbox item."))))

(defun list-org-todos-and-dones ()
  "List all TODO, DONE, and checkbox tasks from the current Org buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name source-buffer))
         (buf (get-buffer-create "*Org Tasks*"))
         (tasks '()))
    ;; Collect TODO/DONE headings with their checkboxes from source buffer
    (with-current-buffer source-buffer
      (org-map-entries
       (lambda ()
         (let* ((heading (org-get-heading t t))
                (state (org-get-todo-state))
                (checkboxes '())
                (heading-line (line-number-at-pos))
                (elem-end (save-excursion (org-end-of-subtree t t) (point))))
           (when (member state '("TODO" "DONE"))
             ;; Collect checkboxes under this heading
             (save-excursion
               (org-back-to-heading t)
               (forward-line 1)
               (while (and (< (point) elem-end)
                          (re-search-forward "^[ \t]*[-+] \\(\\[.\\]\\) \\(.*\\)$" elem-end t))
                 (let ((mark (match-string 1))
                       (text (match-string 2))
                       (cb-line (line-number-at-pos)))
                   (push (list mark text cb-line) checkboxes))))
             (push (list state heading (reverse checkboxes) heading-line) tasks))))))

    (setq tasks (reverse tasks))

    ;; Display results in compilation-mode format
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: compilation; default-directory: %S -*-\n"
                       (file-name-directory source-file)))
        (insert (format "Tasks from: %s\n\n" (buffer-name source-buffer)))
        (insert (propertize "TODO tasks:\n" 'face '(:weight bold :foreground "orange")))
        (insert "\n")
        (let ((has-todos nil))
          (dolist (task tasks)
            (when (equal (car task) "TODO")
              (setq has-todos t)
              (let* ((heading (cadr task))
                     (checkboxes (caddr task))
                     (heading-line (cadddr task)))
                (insert (format "%s:%d:  - [ ] %s\n" source-file heading-line heading))
                (dolist (cb checkboxes)
                  (let ((mark (car cb))
                        (text (cadr cb))
                        (cb-line (caddr cb)))
                    (if (string= mark "[ ]")
                        (insert (format "%s:%d:      - [ ] %s\n" source-file cb-line text))
                      (insert (format "%s:%d:      - [X] %s\n" source-file cb-line text))))))))
          (unless has-todos
            (insert "  (none)\n")))
        (insert "\n")
        (insert (propertize "DONE tasks:\n" 'face '(:weight bold :foreground "green")))
        (insert "\n")
        (let ((has-dones nil))
          (dolist (task tasks)
            (when (equal (car task) "DONE")
              (setq has-dones t)
              (let* ((heading (cadr task))
                     (checkboxes (caddr task))
                     (heading-line (cadddr task)))
                (insert (format "%s:%d:  - [X] %s\n" source-file heading-line heading))
                (dolist (cb checkboxes)
                  (let ((mark (car cb))
                        (text (cadr cb))
                        (cb-line (caddr cb)))
                    (if (string= mark "[ ]")
                        (insert (format "%s:%d:      - [ ] %s\n" source-file cb-line text))
                      (insert (format "%s:%d:      - [X] %s\n" source-file cb-line text))))))))
          (unless has-dones
            (insert "  (none)\n")))
        (compilation-mode)
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;; Key bindings to trigger action
(global-set-key (kbd "C-c C-t") 'insert-org-todo-with-checkboxes)
(global-set-key (kbd "C-c t") 'insert-org-checkbox)
(global-set-key (kbd "C-c C-<return>") 'toggle-org-todo-or-checkbox)
(global-set-key (kbd "C-c l") 'list-org-todos-and-dones)

(provide 'todo-mode)
