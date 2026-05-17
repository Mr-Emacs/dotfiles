;;; storth-mode.el --- Major mode for Storth Programming Language -*- lexical-binding: t; -*-

;; Major mode for Storth Programming language for emacs.

(require 'subr-x)

(defvar storth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?' "\"" table)
    table))

(defun storth--previous-non-empty-line ()
  (save-excursion
    (let ((found nil))
      (while (and (not found) (= (forward-line -1) 0))
        (unless (string-blank-p (thing-at-point 'line t))
          (setq found
                (cons (thing-at-point 'line t)
                      (current-indentation)))))
      found)))

(defun storth-types ()
  '("@i64" "@string" "@i8" "@i16" "@i32" "@u8" "@u16" "@u32" "@u64" "@bool" "@char" "@ptr" "@any"))

(defun storth-keywords ()
  '("pub" "load" "func" "if" "else" "end" "let" "puts" "drop" "dup" "store" "deref" "extern" "do" "while" "not" "const" "struct"))

(defface storth-module-face
  '((t (:inherit font-lock-constant-face)))
  "Face for Storth module namespaces.")

(defun storth-font-lock-keywords ()
  (list
   '("\\<\\([a-zA-Z_][a-zA-Z0-9_]*\\)::"
     (1 'storth-module-face))
   '("\\<func\\>\\s-+\\([a-zA-Z0-9_]+\\)"
     (1 font-lock-function-name-face))
   `(,(regexp-opt (storth-keywords) 'symbols)
     . font-lock-keyword-face)
   `(,(regexp-opt (storth-types))
  . font-lock-type-face)))

(defun storth--find-matching-block ()
  (save-excursion
    (let ((depth 1))
      (while (and (> depth 0) (= (forward-line -1) 0))
        (let ((line (string-trim (thing-at-point 'line t))))
          (cond
           ((string-match-p "\\<end\\>" line)
            (setq depth (1+ depth)))
           ((string-match-p "\\<\\(if\\|func\\|while\\|struct\\)\\>" line)
            (setq depth (1- depth))))))
      (current-indentation))))

(defun storth--desired-indentation ()
  (let* ((indent-len 2)
         (cur-line (string-trim-left (thing-at-point 'line t))))
    (cond
     ((string-match-p "\\<end\\>" cur-line)
      (storth--find-matching-block))
     ((string-match-p "\\<else\\>" cur-line)
      (storth--find-matching-block))
     (t
      (let ((prev (storth--previous-non-empty-line)))
        (if (not prev)
            0
          (let ((prev-line (string-trim-right (car prev)))
                (prev-indent (cdr prev)))
            (if (string-match-p "\\<\\(func\\|if\\|else\\|while\\|do\\|struct\\)\\>" prev-line)
                (+ prev-indent indent-len)
              prev-indent))))))))

(defun storth-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation (storth--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

(defun storth--find-block-match ()
  "Return (BEG . END) of the matching block keyword, or nil."
  (save-excursion
    (let* ((word-start (progn (skip-syntax-backward "w") (point)))
           (word (thing-at-point 'word t)))
      (cond
       ;; On an opening keyword search forward for matching end
       ((member word '("func" "if" "while" "struct"))
        (let ((depth 1))
          (forward-word)
          (while (and (> depth 0)
                      (re-search-forward "\\<\\(func\\|if\\|while\\|struct\\|end\\)\\>" nil t))
            (let ((m (match-string 0)))
              (cond ((member m '("func" "if" "while" "struct")) (setq depth (1+ depth)))
                    ((equal m "end") (setq depth (1- depth))))))
          (when (= depth 0)
            (cons (match-beginning 0) (match-end 0)))))
       ;; On end search backward for matching opener
       ((equal word "end")
        (let ((depth 1))
          (goto-char word-start)
          (while (and (> depth 0)
                      (re-search-backward "\\<\\(func\\|if\\|while\\|struct\\|end\\)\\>" nil t))
            (let ((m (match-string 0)))
              (cond ((equal m "end") (setq depth (1+ depth)))
                    ((member m '("func" "if" "while" "struct")) (setq depth (1- depth))))))
          (when (= depth 0)
            (cons (match-beginning 0) (match-end 0)))))))))

(defun storth--highlight-match ()
  "Highlight the matching block keyword if cursor is on one."
  (let ((match (storth--find-block-match)))
    (if match
        (progn
          (move-overlay storth--match-overlay (car match) (cdr match))
          (overlay-put storth--match-overlay 'face 'show-paren-match)
          (let* ((word-start (save-excursion (skip-syntax-backward "w") (point)))
                 (word-end   (save-excursion (skip-syntax-forward "w") (point))))
            (move-overlay storth--cursor-overlay word-start word-end)
            (overlay-put storth--cursor-overlay 'face 'show-paren-match)))
      (move-overlay storth--match-overlay 1 1)
      (move-overlay storth--cursor-overlay 1 1))))

(defvar-local storth--match-overlay nil)
(defvar-local storth--cursor-overlay nil)

(define-derived-mode storth-mode prog-mode "Storth"
  "Major mode for editing .st files"
  :syntax-table storth-mode-syntax-table
  (setq-local font-lock-defaults (list (storth-font-lock-keywords)))
  (setq-local indent-line-function 'storth-indent-line)
  (setq-local comment-start "// ")
  (setq storth--match-overlay  (make-overlay 1 1))
  (setq storth--cursor-overlay (make-overlay 1 1))
  (add-hook 'post-command-hook #'storth--highlight-match nil t))

(add-to-list 'auto-mode-alist '("\\.st\\'" . storth-mode))

(provide 'storth-mode)

;;; storth-mode.el ends here
