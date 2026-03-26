(require 'seq)
(require 'subr-x)
(require 'diff)

(defvar pn-root-dir "notes")

(defface pn-id-face        '((t :foreground "cyan"    :weight bold)) "Note ID.")
(defface pn-open-face      '((t :foreground "green"   :weight bold)) "OPEN status.")
(defface pn-closed-face    '((t :foreground "red"     :weight bold)) "CLOSED status.")
(defface pn-section-header '((t :weight bold :height 1.2))           "Section header.")
(defface pn-mark-face      '((t :foreground "yellow"  :weight bold)) "Marked note.")
(defface pn-staged-face    '((t :foreground "magenta" :weight bold)) "Staged indicator.")
(defface pn-hunk-add-face  '((t :foreground "green"))                "Diff added line.")
(defface pn-hunk-del-face  '((t :foreground "red"))                  "Diff removed line.")

(defvar-local pn--folded-sections (make-hash-table :test 'equal))
(defvar-local pn--marked nil "List of marked note IDs.")

(defvar pn--staged nil)

(defun pn--root ()
  (expand-file-name pn-root-dir default-directory))

(defun pn--ensure-root ()
  (unless (file-directory-p (pn--root))
    (make-directory (pn--root) t)))

(defun pn--file (dir)
  (expand-file-name "NOTES.md" dir))

(defun pn--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun pn--write-temp (s)
  (let ((f (make-temp-file "pn-")))
    (with-temp-file f (insert s))
    f))

(defun pn--timestamp ()
  (format-time-string "%Y%m%d_%H%M%S"))

(defun pn--slugify (s)
  (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" s)))

(defun pn--note-id (title)
  (format "%s_%s" (pn--slugify title) (pn--timestamp)))

(defun pn-create-note ()
  "Create a new note."
  (interactive)
  (pn--ensure-root)
  (let* ((title   (read-string "Title: "))
         (body    (read-string "Body (optional): "))
         (id      (pn--note-id title))
         (dir     (expand-file-name id (pn--root)))
         (content (format "# %s\n\n- ID: %s\n\n## Priority : 50\n\n## Status : OPEN\n\n## Notes\n\n%s\n"
                          title id body)))
    (make-directory dir t)
    (with-temp-file (pn--file dir) (insert content))
    (pn-status)))

(defun pn--all ()
  (when (file-directory-p (pn--root))
    (seq-filter #'file-directory-p
                (directory-files (pn--root) t "^[^.]"))))

(defun pn--parse (dir)
  (let ((title "") (id "") (status "OPEN") (priority "50") (notes ""))
    (when (file-exists-p (pn--file dir))
      (with-temp-buffer
        (insert-file-contents (pn--file dir))
        (goto-char (point-min))
        (when (re-search-forward "^# \\(.*\\)$" nil t)
          (setq title (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "^- ID: \\(.*\\)$" nil t)
          (setq id (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "Priority[^0-9]*\\([0-9]+\\)" nil t)
          (setq priority (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "Status[[:space:]]*:?[[:space:]]*\\(OPEN\\|CLOSED\\)" nil t)
          (setq status (match-string 1)))
        (goto-char (point-min))
        (when (re-search-forward "^## Notes\n" nil t)
          (setq notes (string-trim (buffer-substring (point) (point-max)))))))
    (unless (string-empty-p id)
      (list :id id :title title :status status :priority priority
            :notes notes :dir dir))))

(defun pn--note-at-point ()
  (let ((id (get-text-property (point) 'pn-id)))
    (when id
      (pn--parse (expand-file-name id (pn--root))))))

(defun pn--stage-change (file old new)
  "Stage a change for FILE, replacing any existing staged change for it."
  (setq pn--staged
        (cons (list :file file :old old :new new)
              (seq-remove (lambda (c) (equal (plist-get c :file) file))
                          pn--staged))))

(defun pn--staged-for (file)
  (seq-find (lambda (c) (equal (plist-get c :file) file)) pn--staged))

(defun pn--edit-field (file regex replacement)
  "Patch FILE by replacing REGEX with REPLACEMENT and stage the result."
  (let* ((on-disk (pn--read-file file))
         (base    (let ((s (pn--staged-for file)))
                    (if s (plist-get s :new) on-disk)))
         (new     (with-temp-buffer
                    (insert base)
                    (goto-char (point-min))
                    (if (re-search-forward regex nil t)
                        (replace-match replacement)
                      (error "Pattern not found: %s" regex))
                    (buffer-string))))
    (pn--stage-change file on-disk new)))

(defun pn-toggle-status ()
  "Stage a status toggle for note at point (or all marked notes)."
  (interactive)
  (dolist (id (or pn--marked
                  (list (plist-get (pn--note-at-point) :id))))
    (when id
      (let* ((dir  (expand-file-name id (pn--root)))
             (file (pn--file dir))
             (data (pn--parse dir)))
        (pn--edit-field
         file
         "## Status : \\(OPEN\\|CLOSED\\)"
         (format "## Status : %s"
                 (if (string= (plist-get data :status) "OPEN") "CLOSED" "OPEN"))))))
  (pn-refresh))

(defun pn-set-priority ()
  "Stage a priority change for note at point (or all marked notes)."
  (interactive)
  (let ((p (read-number "Priority (0-100): ")))
    (dolist (id (or pn--marked
                    (list (plist-get (pn--note-at-point) :id))))
      (when id
        (pn--edit-field
         (pn--file (expand-file-name id (pn--root)))
         "## Priority : [0-9]+"
         (format "## Priority : %d" p))))
    (pn-refresh)))

(defvar-local pn--edit-file nil)
(defvar-local pn--edit-old  nil)

(define-derived-mode pn-edit-mode text-mode "pn-edit"
  "Mode for editing a project note.")

(define-key pn-edit-mode-map (kbd "C-c C-c") #'pn-edit-finish)
(define-key pn-edit-mode-map (kbd "C-c C-k") #'pn-edit-abort)

(defun pn-edit-note ()
  "Edit the NOTES.md for the note at point.
C-c C-c to stage, C-c C-k to abort."
  (interactive)
  (let ((note (pn--note-at-point)))
    (unless note (user-error "No note at point"))
    (let* ((file (pn--file (plist-get note :dir)))
           (old  (pn--read-file file))
           (buf  (get-buffer-create
                  (format "*pn-edit: %s*" (plist-get note :id)))))
      (with-current-buffer buf
        (erase-buffer)
        (insert old)
        (pn-edit-mode)
        (setq pn--edit-file file
              pn--edit-old  old)
        (message "C-c C-c to stage  |  C-c C-k to abort"))
      (pop-to-buffer buf))))

(defun pn-edit-finish ()
  "Stage the edited note and return to the status buffer."
  (interactive)
  (pn--stage-change pn--edit-file pn--edit-old (buffer-string))
  (quit-window t)
  (pn-status))

(defun pn-edit-abort ()
  "Discard edits and return to the status buffer."
  (interactive)
  (quit-window t)
  (pn-status))

(defun pn--diff-string (old new)
  "Return a unified diff string between OLD and NEW."
  (let* ((fa  (pn--write-temp old))
         (fb  (pn--write-temp new))
         (buf (diff-no-select fa fb "-u" 'no-async)))
    (prog1
        (with-current-buffer buf (buffer-string))
      (kill-buffer buf)
      (delete-file fa)
      (delete-file fb))))

(define-derived-mode pn-diff-mode special-mode "pn-diff"
  "Mode for reviewing staged diffs.")

(define-key pn-diff-mode-map (kbd "c") #'pn-commit-all)
(define-key pn-diff-mode-map (kbd "x") #'pn-discard-all)
(define-key pn-diff-mode-map (kbd "q") #'quit-window)

(defun pn-diff-staged ()
  "Show all staged changes."
  (interactive)
  (if (null pn--staged)
      (message "Nothing staged.")
    (let ((buf (get-buffer-create "*pn-staged*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (pn-diff-mode)
          (insert (propertize
                   (format "%d staged change(s) — c:commit  x:discard  q:quit\n\n"
                           (length pn--staged))
                   'face 'bold))
          (dolist (change pn--staged)
            (let* ((file  (plist-get change :file))
                   (old   (plist-get change :old))
                   (new   (plist-get change :new))
                   (diff  (pn--diff-string old new)))
              (insert (propertize (concat "file: " file "\n") 'face 'pn-staged-face))
              (dolist (line (split-string diff "\n"))
                (let ((face (cond ((string-prefix-p "+" line) 'pn-hunk-add-face)
                                  ((string-prefix-p "-" line) 'pn-hunk-del-face)
                                  (t nil))))
                  (insert (if face
                              (propertize (concat line "\n") 'face face)
                            (concat line "\n")))))
              (insert "\n")))))
      (pop-to-buffer buf))))

(defun pn-commit-all ()
  "Write all staged changes to disk."
  (interactive)
  (if (null pn--staged)
      (message "Nothing to commit.")
    (let ((n (length pn--staged)))
      (dolist (change pn--staged)
        (with-temp-file (plist-get change :file)
          (insert (plist-get change :new))))
      (setq pn--staged nil)
      (setq pn--marked nil)
      (message "Committed %d change(s)." n)
      (when (get-buffer "*pn-staged*") (kill-buffer "*pn-staged*"))
      (pn-status))))

(defun pn-discard-all ()
  "Discard all staged changes."
  (interactive)
  (when (yes-or-no-p "Discard all staged changes? ")
    (setq pn--staged nil)
    (message "Staged changes discarded.")
    (when (get-buffer "*pn-staged*") (kill-buffer "*pn-staged*"))
    (pn-refresh)))

(defun pn--insert-header (name)
  (insert (propertize
           (concat name
                   (if (gethash name pn--folded-sections) " ▶" " ▼")
                   "\n")
           'face       'pn-section-header
           'pn-section name)))

(defun pn--insert-note (n)
  (let* ((id       (plist-get n :id))
         (title    (plist-get n :title))
         (status   (plist-get n :status))
         (priority (plist-get n :priority))
         (marked   (member id pn--marked))
         (staged   (pn--staged-for (pn--file (expand-file-name id (pn--root)))))
         (start    (point)))

    (insert (format "%s%s %-36s %-24s P:%-4s %s\n"
                    (if marked "*" " ")
                    (if staged "~" " ")
                    (truncate-string-to-width id 36)
                    (truncate-string-to-width title 24)
                    priority
                    status))

    (add-text-properties start (point) `(pn-id ,id))
    (when marked
      (add-text-properties start (point) '(face pn-mark-face)))

    (let ((id-end (min (+ start 2 (length id)) (point))))
      (add-text-properties (+ start 2) id-end '(face pn-id-face)))

    (let ((s (- (point) (length status) 1)))
      (add-text-properties s (1- (point))
                           `(face ,(if (string= status "OPEN")
                                       'pn-open-face
                                     'pn-closed-face))))))

(defun pn-refresh ()
  "Redraw the *Project Notes* buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Project Notes\n" 'face '(:weight bold :height 1.4)))
    (insert (format " %d staged change(s)  —  d:review  C:commit  X:discard\n\n"
                    (length pn--staged)))

    (let* ((all   (delq nil (mapcar #'pn--parse (pn--all))))
           (by-p  (lambda (a b)
                    (> (string-to-number (plist-get a :priority))
                       (string-to-number (plist-get b :priority)))))
           (open  (sort (seq-filter
                         (lambda (n) (string= (plist-get n :status) "OPEN")) all)
                        by-p))
           (closed(sort (seq-filter
                         (lambda (n) (string= (plist-get n :status) "CLOSED")) all)
                        by-p)))

      (pn--insert-header "OPEN")
      (unless (gethash "OPEN" pn--folded-sections)
        (dolist (n open) (pn--insert-note n)))

      (insert "\n")
      (pn--insert-header "CLOSED")
      (unless (gethash "CLOSED" pn--folded-sections)
        (dolist (n closed) (pn--insert-note n))))))

(defun pn-toggle-section ()
  (interactive)
  (let ((sec (get-text-property (point) 'pn-section)))
    (when sec
      (puthash sec (not (gethash sec pn--folded-sections)) pn--folded-sections)
      (pn-refresh))))

(defun pn-mark ()
  (interactive)
  (let ((id (get-text-property (point) 'pn-id)))
    (when id
      (unless (member id pn--marked) (push id pn--marked))
      (forward-line 1)
      (pn-refresh))))

(defun pn-unmark ()
  (interactive)
  (let ((id (get-text-property (point) 'pn-id)))
    (setq pn--marked (remove id pn--marked))
    (forward-line 1)
    (pn-refresh)))

(defun pn-unmark-all ()
  (interactive)
  (setq pn--marked nil)
  (pn-refresh))

(define-derived-mode pn-mode special-mode "pn"
  "Major mode for Project Notes.")

(define-key pn-mode-map (kbd "g")   #'pn-refresh)
(define-key pn-mode-map (kbd "TAB") #'pn-toggle-section)
(define-key pn-mode-map (kbd "m")   #'pn-mark)
(define-key pn-mode-map (kbd "u")   #'pn-unmark)
(define-key pn-mode-map (kbd "U")   #'pn-unmark-all)
(define-key pn-mode-map (kbd "s")   #'pn-toggle-status)
(define-key pn-mode-map (kbd "p")   #'pn-set-priority)
(define-key pn-mode-map (kbd "e")   #'pn-edit-note)
(define-key pn-mode-map (kbd "n")   #'pn-create-note)
(define-key pn-mode-map (kbd "d")   #'pn-diff-staged)
(define-key pn-mode-map (kbd "C")   #'pn-commit-all)
(define-key pn-mode-map (kbd "X")   #'pn-discard-all)

(defun pn-status ()
  "Open (or refresh) the Project Notes status buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Project Notes*")))
    (with-current-buffer buf
      (pn-mode)
      (pn-refresh))
    (pop-to-buffer buf)))

(provide 'project-notes)
;;; project-notes.el ends here
