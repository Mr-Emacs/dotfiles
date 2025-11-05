;;; cgoogle.el --- Hoogle-style search for C functions -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: xsoder
;; Keywords: c, search, tools, ctags
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; cgoogle provides Hoogle-style searching for C functions.
;;
;; REQUIRES universal-ctags:
;;   macOS: brew install universal-ctags
;;   Linux: sudo apt install universal-ctags
;;
;; Usage:
;;   M-x cgoogle-index-file     - Index a C/H file
;;   M-x cgoogle-search         - Search by name or signature
;;   M-x cgoogle-check-ctags    - Verify ctags

(require 'cl-lib)

(defgroup cgoogle nil
  "Hoogle-style search for C functions."
  :group 'tools
  :prefix "cgoogle-")

(defcustom cgoogle-ctags-program "ctags"
  "Path to ctags executable."
  :type 'string
  :group 'cgoogle)

(defcustom cgoogle-debug nil
  "Enable debug logging."
  :type 'boolean
  :group 'cgoogle)

(defcustom cgoogle-auto-index-directories
  nil
  "Directories to auto-index on first use.
Set to list of directories to enable auto-indexing.

WARNING: Auto-indexing can be slow (minutes) for large directory trees.
Recommended: Use `cgoogle-index-stdlib-quick' instead for fast standard library indexing.

Example:
  (setq cgoogle-auto-index-directories '(\"/usr/include\"))"
  :type '(repeat directory)
  :group 'cgoogle)

(defcustom cgoogle-auto-index-patterns
  '("\\.h$")
  "File patterns to match when auto-indexing directories.
Only files matching these patterns will be indexed."
  :type '(repeat string)
  :group 'cgoogle)

(defcustom cgoogle-auto-index-on-load nil
  "Whether to automatically index standard directories on first search.
DISABLED BY DEFAULT because indexing /usr/include can take minutes.

Recommended alternative: M-x cgoogle-index-stdlib-quick
This indexes only essential standard headers (~1 second).

To enable auto-indexing:
  (setq cgoogle-auto-index-on-load t)
  (setq cgoogle-auto-index-directories '(\"/usr/include\"))"
  :type 'boolean
  :group 'cgoogle)

(defcustom cgoogle-stdlib-essential-headers
  '("stdio.h" "stdlib.h" "string.h" "math.h" "time.h"
    "stddef.h" "stdint.h" "stdbool.h" "stdarg.h"
    "ctype.h" "errno.h" "assert.h" "limits.h" "float.h"
    "signal.h" "setjmp.h" "locale.h")
  "Essential C standard library headers to index quickly.
Used by `cgoogle-index-stdlib-quick' for fast startup."
  :type '(repeat string)
  :group 'cgoogle)

(defcustom cgoogle-index-stdlib-on-load nil
  "Whether to automatically index essential standard library headers on load.
When non-nil, runs `cgoogle-index-stdlib-quick' when cgoogle is first loaded.
This is MUCH faster than full auto-indexing (seconds vs minutes).

Recommended: Enable this for instant access to standard library functions.

Alternative: Run M-x cgoogle-index-stdlib-quick manually when needed."
  :type 'boolean
  :group 'cgoogle)

(defvar cgoogle--auto-indexed nil
  "Whether auto-indexing has been performed.")

(defvar cgoogle-functions nil
  "Indexed C functions.
List of plists with :name :return-type :params :file :line :signature.")

(defvar cgoogle-history nil
  "Search query history.")

(defun cgoogle--log (format-string &rest args)
  "Log message when `cgoogle-debug' is non-nil."
  (when cgoogle-debug
    (apply #'message (concat "[cgoogle] " format-string) args)))

(defun cgoogle--ctags-available-p ()
  "Check if universal-ctags is available."
  (when-let ((ctags-path (executable-find cgoogle-ctags-program)))
    (let ((version-output (shell-command-to-string
                          (format "%s --version 2>&1" cgoogle-ctags-program))))
      (string-match-p "Universal Ctags" version-output))))

(defun cgoogle--run-ctags (file)
  "Run ctags on FILE and return tags as list of parsed entries."
  (unless (cgoogle--ctags-available-p)
    (error "Universal ctags not found. Install: brew install universal-ctags"))

  (let* ((abs-file (expand-file-name file))
         (temp-tags (make-temp-file "cgoogle-tags-"))
         (default-directory (file-name-directory abs-file))
         (filename (file-name-nondirectory abs-file)))

    (cgoogle--log "Running ctags on %s" filename)

    (unwind-protect
        (let ((exit-code
               (call-process cgoogle-ctags-program nil nil nil
                           "-f" temp-tags
                           "--fields=+neKStz"      ; All useful fields
                           "--kinds-c=fp"          ; Only functions and prototypes
                           "--extras=-F"           ; No file tags
                           "--language-force=C"    ; Force C
                           "--output-format=json"  ; JSON output
                           "--sort=no"             ; Don't sort (faster)
                           filename)))

          (cgoogle--log "ctags exit code: %d" exit-code)

          (if (zerop exit-code)
              (cgoogle--parse-ctags-json-file temp-tags abs-file)
            (error "ctags failed with exit code %d" exit-code)))

      ;; Cleanup
      (when (file-exists-p temp-tags)
        (delete-file temp-tags)))))

(defun cgoogle--parse-ctags-json-file (tags-file original-file)
  "Parse ctags JSON output from TAGS-FILE for ORIGINAL-FILE."
  (with-temp-buffer
    (insert-file-contents tags-file)
    (goto-char (point-min))
    (let ((functions nil))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
          (when-let ((func (cgoogle--parse-json-line line original-file)))
            (push func functions)))
        (forward-line 1))
      (cgoogle--log "Parsed %d functions from ctags" (length functions))
      (nreverse functions))))

(defun cgoogle--parse-json-line (line file)
  "Parse single JSON LINE from ctags output for FILE."
  (when (and (> (length line) 2)
             (string-prefix-p "{" line))
    (condition-case err
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json-key-type 'keyword)
               (json-false nil)
               (data (json-read-from-string line))
               (name (plist-get data :name))
               (kind (plist-get data :kind))
               (line-num (plist-get data :line))
               (signature (plist-get data :signature))
               (typeref (plist-get data :typeref)))

          ;; Only process functions and prototypes
          (when (member kind '("function" "prototype"))
            (cgoogle--log "Found: %s %s%s"
                         (or typeref "void") name (or signature "()"))

            (let* ((return-type (when typeref
                                 (if (string-match "^[^:]+:\\s*\\(.+\\)" typeref)
                                     (match-string 1 typeref)
                                   typeref)))
                   (params (cgoogle--parse-params signature))
                   (context (format "%s %s%s"
                                  (or return-type "void")
                                  name
                                  (or signature "()"))))

              (list :name name
                    :file file
                    :line (or line-num 1)
                    :signature signature
                    :return-type (or return-type "void")
                    :params params
                    :context context))))
      (error
       (cgoogle--log "Failed to parse JSON line: %s (error: %s)" line err)
       nil))))

(defun cgoogle--parse-params (signature)
  "Extract parameter types from SIGNATURE like '(int x, char *y)'.
Returns list of type strings."
  (when (and signature (string-match "(\\(.*\\))" signature))
    (let ((params-str (match-string 1 signature)))
      (unless (or (string-empty-p (string-trim params-str))
                  (string-match-p "^\\s-*void\\s-*$" params-str))
        (mapcar #'cgoogle--extract-type
                (split-string params-str "," t "[ \t\n]"))))))

(defun cgoogle--extract-type (param-decl)
  "Extract type from PARAM-DECL like 'const char *name' -> 'const char *'."
  (let ((trimmed (string-trim param-decl)))
    (if (string-match "\\(.*?\\)\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\s-*\\(\\[.*\\]\\)?$" trimmed)
        (string-trim (match-string 1 trimmed))
      trimmed)))

(defun cgoogle--normalize-type (type)
  "Normalize TYPE for flexible matching.
Removes qualifiers, normalizes whitespace and pointers."
  (when type
    (let ((norm type))
      (setq norm (replace-regexp-in-string
                 "\\<\\(const\\|volatile\\|restrict\\|static\\|inline\\|extern\\)\\>"
                 "" norm))
      (setq norm (replace-regexp-in-string "\\s-+" " " norm))
      (setq norm (replace-regexp-in-string "\\s-*\\*" "*" norm))
      (setq norm (replace-regexp-in-string "\\*\\s-+" "*" norm))
      (downcase (string-trim norm)))))

(defun cgoogle--should-auto-index-p ()
  "Return t if auto-indexing should be performed."
  (and cgoogle-auto-index-on-load
       (not cgoogle--auto-indexed)
       cgoogle-auto-index-directories
       (cgoogle--ctags-available-p)))

(defun cgoogle-index-stdlib-quick ()
  "Quickly index essential C standard library headers.
This is MUCH faster than indexing all of /usr/include (seconds vs minutes).

Indexes only essential headers like stdio.h, stdlib.h, string.h, etc.
Configure with `cgoogle-stdlib-essential-headers'.

This is the recommended way to get standard library function search."
  (interactive)
  (unless (cgoogle--ctags-available-p)
    (error "Universal ctags not found. Install: brew install universal-ctags"))

  (let ((total-funcs 0)
        (indexed-files 0)
        (failed-files 0)
        (start-time (current-time))
        (include-dirs (list "/usr/include"
                           "/usr/local/include"
                           (when (eq system-type 'darwin)
                             (let ((sdk (ignore-errors
                                         (string-trim
                                          (shell-command-to-string
                                           "xcrun --show-sdk-path 2>/dev/null")))))
                               (when (and sdk (not (string-empty-p sdk)))
                                 (concat sdk "/usr/include")))))))

    (message "Quick-indexing %d essential standard headers..."
             (length cgoogle-stdlib-essential-headers))

    (dolist (header cgoogle-stdlib-essential-headers)
      (let ((found nil))
        (dolist (dir include-dirs)
          (when (and dir (not found))
            (let ((path (expand-file-name header dir)))
              (when (file-exists-p path)
                (condition-case err
                    (let ((funcs (cgoogle--run-ctags path)))
                      (setq cgoogle-functions (append cgoogle-functions funcs))
                      (setq total-funcs (+ total-funcs (length funcs)))
                      (setq indexed-files (+ indexed-files 1))
                      (setq found t))
                  (error
                   (cgoogle--log "Error indexing %s: %s" path err)
                   (setq failed-files (+ failed-files 1))))))))
        (unless found
          (cgoogle--log "Warning: Could not find %s" header))))

    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (message "Indexed %d functions from %d standard headers in %.1fs"
               total-funcs indexed-files elapsed))

    total-funcs))

(defun cgoogle-auto-index ()
  "Index standard directories automatically.
Indexes directories in `cgoogle-auto-index-directories' (typically /usr/include).
This allows searching for standard library functions without manual indexing.

WARNING: This can take several minutes for large directory trees.
Consider using `cgoogle-index-stdlib-quick' instead for faster startup."
  (interactive)
  (when (cgoogle--ctags-available-p)
    (let ((total-files 0)
          (total-funcs 0)
          (start-time (current-time)))

      (message "Auto-indexing standard libraries (this may take a while)...")

      (dolist (dir cgoogle-auto-index-directories)
        (when (and dir (file-directory-p dir))
          (cgoogle--log "Auto-indexing directory: %s" dir)
          (let ((before-count (length cgoogle-functions))
                (pattern (mapconcat #'identity cgoogle-auto-index-patterns "\\|")))

            (condition-case err
                (let ((files (directory-files-recursively dir pattern)))
                  (setq total-files (+ total-files (length files)))
                  (message "Indexing %d files from %s..." (length files) dir)

                  (dolist (file files)
                    (condition-case file-err
                        (let ((funcs (cgoogle--run-ctags file)))
                          (setq cgoogle-functions (append cgoogle-functions funcs)))
                      (error
                       (cgoogle--log "Error indexing %s: %s" file file-err)))))
              (error
               (message "Warning: Could not index %s: %s" dir err))))))

      (setq total-funcs (length cgoogle-functions))
      (setq cgoogle--auto-indexed t)

      (let ((elapsed (float-time (time-subtract (current-time) start-time))))
        (message "Auto-indexed %d functions from %d files in %.1fs"
                 total-funcs total-files elapsed))

      total-funcs)))

(defun cgoogle-reset-auto-index ()
  "Reset auto-indexing flag so it runs again next time."
  (interactive)
  (setq cgoogle--auto-indexed nil)
  (message "Auto-indexing reset. Will run on next search."))

;;; Indexing

(defun cgoogle--remove-file-entries (file)
  "Remove all entries for FILE from index.
Returns number of entries removed."
  (let* ((abs-file (expand-file-name file))
         (old-count (length cgoogle-functions))
         (filtered (cl-remove-if
                   (lambda (func)
                     (string= (expand-file-name (plist-get func :file)) abs-file))
                   cgoogle-functions)))
    (setq cgoogle-functions filtered)
    (- old-count (length cgoogle-functions))))

(defun cgoogle-index-file (file)
  "Index C functions from FILE using ctags.
If file was previously indexed, replaces old entries."
  (interactive "fC/H file to index: ")

  (let ((abs-file (expand-file-name file)))
    (unless (file-exists-p abs-file)
      (error "File not found: %s" abs-file))

    (message "Indexing %s with ctags..." (file-name-nondirectory abs-file))

    (let* ((old-count (length cgoogle-functions))
           (removed (cgoogle--remove-file-entries abs-file))
           (new-funcs (cgoogle--run-ctags abs-file))
           (count (length new-funcs)))

      (setq cgoogle-functions (append cgoogle-functions new-funcs))

      (if (> removed 0)
          (message "Re-indexed %s: removed %d old, added %d new (total: %d)"
                   (file-name-nondirectory abs-file)
                   removed
                   count
                   (length cgoogle-functions))
        (message "Indexed %d functions from %s (total: %d)"
                 count
                 (file-name-nondirectory abs-file)
                 (length cgoogle-functions)))

      (when (zerop count)
        (message "Warning: No functions found. Check file has function declarations.")))))

(defun cgoogle-index-directory (dir &optional pattern)
  "Index all C files in DIR matching PATTERN (default: *.c *.h).
Files already indexed will be re-indexed (replacing old entries)."
  (interactive "DDirectory to index: ")
  (let* ((pattern (or pattern "\\.[ch]$"))
         (files (directory-files-recursively dir pattern))
         (total-added 0)
         (total-replaced 0))

    (message "Indexing %d files from %s..." (length files) dir)

    (dolist (file files)
      (let ((before (length cgoogle-functions)))
        (condition-case err
            (progn
              (cgoogle-index-file file)
              (let ((after (length cgoogle-functions))
                    (delta (- after before)))
                (if (>= delta 0)
                    (setq total-added (+ total-added delta))
                  (setq total-replaced (+ total-replaced 1)))))
          (error (message "Error indexing %s: %s" file err)))))

    (message "Done: %d functions from %d files (total: %d)"
             total-added
             (length files)
             (length cgoogle-functions))))

(defun cgoogle-clear-index ()
  "Clear all indexed functions.
This also resets the auto-indexing flag, so standard directories
will be re-indexed on next search."
  (interactive)
  (setq cgoogle-functions nil)
  (setq cgoogle--auto-indexed nil)
  (message "Index cleared (will auto-index on next search)"))

(defun cgoogle-remove-file (file)
  "Remove FILE from index."
  (interactive "fRemove file from index: ")
  (let* ((abs-file (expand-file-name file))
         (removed (cgoogle--remove-file-entries abs-file)))
    (if (> removed 0)
        (message "Removed %d functions from %s (total: %d remaining)"
                 removed
                 (file-name-nondirectory abs-file)
                 (length cgoogle-functions))
      (message "File not in index: %s" (file-name-nondirectory abs-file)))))

(defun cgoogle--parse-query (query)
  "Parse QUERY into search criteria.
Returns plist with :name :types :return-type.

Query formats:
  'funcName'                 -> search by name
  'Type1, Type2'             -> search by param types
  'Type1, Type2 -> RetType'  -> params + return type (arrow syntax)
  'Type1 Type2'              -> search by param types
  'ReturnType func'          -> search by return + name"
  (let* ((trimmed (string-trim query))
         (has-arrow (string-match-p "->" trimmed)))

    (cond
     (has-arrow
      (let* ((parts (split-string trimmed "->" t "\\s-+"))
             (param-part (string-trim (car parts)))
             (return-part (when (cadr parts) (string-trim (cadr parts)))))
        (list :types (if (string-match-p "," param-part)
                        (mapcar #'cgoogle--normalize-type
                                (split-string param-part "," t "\\s-+"))
                      (mapcar #'cgoogle--normalize-type
                              (split-string param-part nil t)))
              :return-type (when return-part
                            (cgoogle--normalize-type return-part)))))

     ((string-match-p "," trimmed)
      (list :types (mapcar #'cgoogle--normalize-type
                          (split-string trimmed "," t "\\s-+"))))

     ((= (length (split-string trimmed nil t)) 1)
      (list :name trimmed
            :types (list (cgoogle--normalize-type trimmed))))

     (t
      (let* ((parts (split-string trimmed nil t))
             (last-word (car (last parts)))
             (other-words (butlast parts)))
        (if (string-match-p "^[a-z_]" last-word)
            (list :name last-word
                  :return-type (when other-words
                                (cgoogle--normalize-type
                                 (mapconcat #'identity other-words " "))))
          (list :types (mapcar #'cgoogle--normalize-type parts))))))))

(defun cgoogle--match-name (query-name func)
  "Check if QUERY-NAME matches FUNC name."
  (let ((name (plist-get func :name)))
    (or (string-match-p (regexp-quote query-name) name)
        (string-prefix-p query-name name t))))

(defun cgoogle--match-types (query-types func)
  "Check if all QUERY-TYPES appear in FUNC signature."
  (let* ((return-type (cgoogle--normalize-type (plist-get func :return-type)))
         (param-types (mapcar #'cgoogle--normalize-type (plist-get func :params)))
         (all-types (cons return-type param-types)))

    (cl-every
     (lambda (qtype)
       (cl-some
        (lambda (ftype)
          (or (string-match-p (regexp-quote qtype) ftype)
              (string-match-p (regexp-quote ftype) qtype)
              (string= (replace-regexp-in-string "\\*" "" qtype)
                      (replace-regexp-in-string "\\*" "" ftype))))
        all-types))
     query-types)))

(defun cgoogle--match-function (query-plist func)
  "Check if FUNC matches QUERY-PLIST."
  (let ((query-name (plist-get query-plist :name))
        (query-types (plist-get query-plist :types))
        (query-return (plist-get query-plist :return-type)))
    (if (and query-name query-types (not query-return))
        (or (cgoogle--match-name query-name func)
            (cgoogle--match-types query-types func))
      (and
       (or (not query-name)
           (cgoogle--match-name query-name func))
       (or (not query-return)
           (string-match-p (regexp-quote query-return)
                          (cgoogle--normalize-type (plist-get func :return-type))))
       (or (not query-types)
           (cgoogle--match-types query-types func))))))

(defun cgoogle--search (query)
  "Search indexed functions for QUERY."
  (if (null cgoogle-functions)
      (progn
        (message "No functions indexed. Run M-x cgoogle-index-file first.")
        nil)
    (let ((query-plist (cgoogle--parse-query query)))
      (cgoogle--log "Searching with: %S" query-plist)
      (cl-remove-if-not
       (lambda (func) (cgoogle--match-function query-plist func))
       cgoogle-functions))))

(defun cgoogle--format-function-for-compilation (func)
  "Format FUNC for compilation-mode.
Returns string in format: file:line: signature"
  (let ((file (plist-get func :file))
        (line (plist-get func :line))
        (context (plist-get func :context)))
    (format "%s:%d: %s" file line context)))

(defun cgoogle--display-results (query results)
  "Display RESULTS for QUERY in compilation-mode buffer."
  (let ((buf (get-buffer-create "*cgoogle-results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-*- mode: compilation; default-directory: %S -*-\n"
                       default-directory))
        (insert (format "CGoogle search: %s\n" query))
        (insert (format "Found %d match%s\n\n"
                       (length results)
                       (if (= (length results) 1) "" "es")))

        (if (null results)
            (insert "No matches found.\n\n"
                   "Try:\n"
                   "  - Simpler query (just function name)\n"
                   "  - M-x cgoogle-show-indexed to see what's indexed\n"
                   "  - M-x cgoogle-show-stats for index info\n")

          (dolist (func results)
            (insert (cgoogle--format-function-for-compilation func))
            (insert "\n")))

        (compilation-mode)
        (setq-local compilation-error-regexp-alist
                   '(("^\\([^:\n]+\\):\\([0-9]+\\):" 1 2)))
        (goto-char (point-min))
        (when (re-search-forward "^[^-].*:[0-9]+:" nil t)
          (beginning-of-line))))

    (display-buffer buf)
    (message "Use 'n' and 'p' to navigate, RET to jump, 'q' to quit")))

(defun cgoogle--goto-function (func)
  "Jump to FUNC definition."
  (when-let ((file (plist-get func :file))
             (line (plist-get func :line)))
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun cgoogle-search (query)
  "Search indexed functions for QUERY.

Query syntax:
  'funcName'                 - by name
  'Color, float'             - by parameter types
  'Color, float -> Color'    - by params + return type (arrow syntax)
  'int func'                 - by return type and name"

  (interactive (list (read-string "Search: " nil 'cgoogle-history)))

  (when (cgoogle--should-auto-index-p)
    (cgoogle-auto-index))

  (let ((results (cgoogle--search query)))
    (cgoogle--display-results query results)))

(defun cgoogle-search-at-point ()
  "Search for symbol at point."
  (interactive)
  (if-let ((sym (thing-at-point 'symbol t)))
      (cgoogle-search sym)
    (call-interactively #'cgoogle-search)))

(defun cgoogle-check-ctags ()
  "Check ctags installation."
  (interactive)
  (if-let ((ctags (executable-find cgoogle-ctags-program)))
      (let ((version (shell-command-to-string
                     (format "%s --version 2>&1" cgoogle-ctags-program))))
        (if (string-match-p "Universal Ctags" version)
            (message "Universal Ctags found:\n%s\nPath: %s" version ctags)
          (message "Found ctags but not Universal Ctags:\n%s\n\nInstall" version)))
    (message "ctags not found\n\nInstall:\n  Linux: sudo apt install universal-ctags")))

(defun cgoogle-show-stats ()
  "Show index statistics."
  (interactive)
  (let* ((files (delete-dups (mapcar (lambda (f) (plist-get f :file))
                                     cgoogle-functions)))
         (file-count (length files))
         (auto-indexed-str (if cgoogle--auto-indexed " [auto-indexed]" "")))
    (message "Indexed: %d functions from %d file%s%s | Parser: %s"
             (length cgoogle-functions)
             file-count
             (if (= file-count 1) "" "s")
             auto-indexed-str
             (if (cgoogle--ctags-available-p) "ctags" "NONE"))))

(defun cgoogle-show-files ()
  "Show which files are currently indexed."
  (interactive)
  (let* ((files (delete-dups (mapcar (lambda (f) (plist-get f :file))
                                     cgoogle-functions)))
         (file-stats (mapcar
                     (lambda (file)
                       (let ((count (cl-count-if
                                    (lambda (f) (string= (plist-get f :file) file))
                                    cgoogle-functions)))
                         (cons file count)))
                     files))
         (buf (get-buffer-create "*cgoogle-files*")))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Indexed Files (%d total, %d functions)\n\n"
                       (length files)
                       (length cgoogle-functions)))

        (if (null files)
            (insert "No files indexed yet.\n\nRun: M-x cgoogle-index-file\n")

          (dolist (stat file-stats)
            (let ((file (car stat))
                  (count (cdr stat)))
              (insert (format "%4d functions  %s\n"
                             count
                             (abbreviate-file-name file))))))

        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun cgoogle-show-indexed ()
  "Show all indexed functions."
  (interactive)
  (let ((buf (get-buffer-create "*cgoogle-index*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Total: %d functions\n\n" (length cgoogle-functions)))

        (if (null cgoogle-functions)
            (insert "No functions indexed.\n\nRun: M-x cgoogle-index-file\n")

          (dolist (func cgoogle-functions)
            (insert (format "%s\n" (plist-get func :context)))
            (insert (format "  File: %s:%d\n"
                           (file-name-nondirectory (plist-get func :file))
                           (plist-get func :line)))
            (insert (format "  Params: %s\n\n" (plist-get func :params)))))

        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun cgoogle-debug-file (file)
  "Debug why FILE isn't indexing correctly."
  (interactive "fDebug file: ")
  (let ((cgoogle-debug t))
    (message "Debugging %s" file)
    (cgoogle-check-ctags)
    (message "Attempting to index...")
    (condition-case err
        (let ((funcs (cgoogle--run-ctags file)))
          (message "Success! Found %d functions" (length funcs))
          (when funcs
            (message "First function: %s" (plist-get (car funcs) :context))))
      (error (message "Error: %s" err)))))

(when cgoogle-index-stdlib-on-load
  (cgoogle-index-stdlib-quick))

(provide 'cgoogle)

;;; cgoogle.el ends here
