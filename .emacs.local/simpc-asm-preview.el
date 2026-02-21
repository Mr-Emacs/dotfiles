;;; simpc-asm-preview.el --- Show compiler ASM output without writing .s files -*- lexical-binding: t; -*-

;; Usage:
;;   (require 'simpc-asm-preview)
;;   (add-hook 'simpc-mode-hook #'simpc-asm-preview-mode)   ; auto-attach
;;   Or just  M-x simpc-asm-preview-mode  to toggle on demand.
;;
;; Keybinds (active when the minor mode is on):
;;   C-c a s   – show ASM for the whole buffer
;;   C-c a r   – show ASM for the current region
;;   C-c a q   – close the ASM preview window

;;; ---- Customisation ---------------------------------------------------------

(defgroup simpc-asm-preview nil
  "Inline ASM preview for C buffers."
  :group 'programming)

(defcustom simpc-asm-preview-compiler "gcc"
  "Compiler binary to use.  Can be an absolute path or anything on PATH.
Common values: \"gcc\", \"clang\", \"g++\", \"clang++\"."
  :type 'string
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-flags '("-O2" "-std=c11" "-Wall")
  "Extra flags passed to the compiler.
The flags -S -o - (compile to ASM, output to stdout) are always appended
automatically – do NOT add them here."
  :type '(repeat string)
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-buffer-name "*C ASM Preview*"
  "Name of the buffer used to display assembly output."
  :type 'string
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-intel-syntax t
  "When non-nil pass -masm=intel to the compiler (Intel syntax).
Set to nil for AT&T syntax."
  :type 'boolean
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-strip-directives t
  "When non-nil, filter out assembler directives and labels that clutter
the output (lines starting with ., CFI annotations, etc.).
Useful for focusing on the actual instructions."
  :type 'boolean
  :group 'simpc-asm-preview)

;;; ---- Internal helpers ------------------------------------------------------

(defun simpc-asm-preview--build-command ()
  "Return the compiler invocation as a list of strings."
  (append
   (list simpc-asm-preview-compiler)
   simpc-asm-preview-flags
   (when simpc-asm-preview-intel-syntax '("-masm=intel"))
   ;; -S  → compile to assembly
   ;; -x c → treat stdin as C (needed when reading from a pipe)
   ;; -o - → write assembly to stdout
   '("-S" "-x" "c" "-o" "-" "-")))

(defun simpc-asm-preview--strip (asm)
  "Remove noisy directives and mangled labels from ASM string."
  (let ((lines (split-string asm "\n"))
        (kept  '()))
    (dolist (line lines)
      (unless (or (string-match-p "^\\s-*\\.\\w" line)   ; .cfi, .section, .type etc.
                  (string-match-p "^\\s-*#" line)         ; preprocessor noise
                  (string-match-p "^\"\\?\\?" line)       ; MSVC/clang mangled string literals
                  (string-match-p "^\\.LC[0-9]+:" line))  ; GCC/clang rodata labels
        (push line kept)))
    (string-join (nreverse kept) "\n")))

(defun simpc-asm-preview--run (source-string)
  "Compile SOURCE-STRING in memory and return the ASM output as a string.
Signals a `user-error' if compilation fails, showing the error output."
  (let* ((cmd      (simpc-asm-preview--build-command))
         (err-file (make-temp-file "simpc-asm-err"))
         (out-buf  (generate-new-buffer " *simpc-asm-out*"))
         exit-code asm err)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert source-string)
            (setq exit-code
                  (apply #'call-process-region
                         (point-min) (point-max)
                         (car cmd)
                         nil                    ; don't delete region
                         (list out-buf err-file) ; stdout=buffer, stderr=filename (required by Emacs)
                         nil                    ; no display
                         (cdr cmd))))
          (setq asm (with-current-buffer out-buf (buffer-string)))
          (setq err (with-temp-buffer
                      (insert-file-contents err-file)
                      (buffer-string))))
      (kill-buffer out-buf)
      (delete-file err-file))
    (if (zerop exit-code)
        asm
      (user-error "Compiler error:\n%s" err))))

(defun simpc-asm-preview--display (asm-string source-desc)
  "Pop up or refresh the preview buffer with ASM-STRING.
Reuses an existing window if one is already showing the buffer,
or the other window in a 2-window frame, otherwise splits."
  (let ((cleaned (if simpc-asm-preview-strip-directives
                     (simpc-asm-preview--strip asm-string)
                   asm-string))
        (buf (get-buffer-create simpc-asm-preview-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "; Assembly for: %s\n" source-desc))
        (insert (format "; Compiler:     %s %s\n"
                        simpc-asm-preview-compiler
                        (string-join simpc-asm-preview-flags " ")))
        (insert (format "; Syntax:       %s\n\n"
                        (if simpc-asm-preview-intel-syntax "Intel" "AT&T")))
        (insert cleaned))
      (if (fboundp 'asm-mode) (asm-mode) (prog-mode))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    ;; Display logic: reuse existing window > other window in split > new split
    (let ((existing-win (get-buffer-window buf)))
      (cond
       ;; Already visible somewhere – just update it in place
       (existing-win
        (set-window-buffer existing-win buf))
       ;; Exactly 2 windows: put it in the other one without splitting again
       ((= (length (window-list)) 2)
        (set-window-buffer (next-window) buf))
       ;; No split yet: open a side window
       (t
        (display-buffer buf
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.45)))))
      ;; Return point to top in whichever window is showing it
      (when-let ((win (get-buffer-window buf)))
        (set-window-point win (point-min))))))

;;; ---- Interactive commands --------------------------------------------------

;;;###autoload
(defun simpc-asm-preview-buffer ()
  "Compile the current buffer's C code and display the generated assembly.
No file is written; the source is piped directly to the compiler."
  (interactive)
  (let* ((source (buffer-string))
         (desc   (or (buffer-file-name) (buffer-name)))
         (asm    (simpc-asm-preview--run source)))
    (simpc-asm-preview--display asm desc)))

;;;###autoload
(defun simpc-asm-preview-region (beg end)
  "Compile the selected region's C code and display the assembly."
  (interactive "r")
  (let* ((source (buffer-substring-no-properties beg end))
         (asm    (simpc-asm-preview--run source)))
    (simpc-asm-preview--display asm "region")))

(defun simpc-asm-preview-close ()
  "Close the ASM preview side window."
  (interactive)
  (when-let ((win (get-buffer-window simpc-asm-preview-buffer-name)))
    (delete-window win)))

;;; ---- Minor mode ------------------------------------------------------------

(defvar simpc-asm-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'simpc-asm-preview-buffer)
    (define-key map (kbd "C-c a r") #'simpc-asm-preview-region)
    (define-key map (kbd "C-c a q") #'simpc-asm-preview-close)
    map)
  "Keymap for `simpc-asm-preview-mode'.")

;;;###autoload
(define-minor-mode simpc-asm-preview-mode
  "Minor mode that lets you preview compiler-generated assembly on the fly.

\\{simpc-asm-preview-mode-map}"
  :lighter " ASMprev"
  :keymap  simpc-asm-preview-mode-map
  :group   'simpc-asm-preview)

(provide 'simpc-asm-preview)

(defgroup simpc-asm-preview nil
  "Inline ASM preview for C buffers."
  :group 'programming)

(defcustom simpc-asm-preview-compiler "gcc"
  "Compiler binary to use.  Can be an absolute path or anything on PATH.
Common values: \"gcc\", \"clang\", \"g++\", \"clang++\"."
  :type 'string
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-flags '("-O2" "-std=c11" "-Wall")
  "Extra flags passed to the compiler.
The flags -S -o - (compile to ASM, output to stdout) are always appended
automatically – do NOT add them here."
  :type '(repeat string)
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-buffer-name "*C ASM Preview*"
  "Name of the buffer used to display assembly output."
  :type 'string
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-intel-syntax t
  "When non-nil pass -masm=intel to the compiler (Intel syntax).
Set to nil for AT&T syntax."
  :type 'boolean
  :group 'simpc-asm-preview)

(defcustom simpc-asm-preview-strip-directives t
  "When non-nil, filter out assembler directives and labels that clutter
the output (lines starting with ., CFI annotations, etc.).
Useful for focusing on the actual instructions."
  :type 'boolean
  :group 'simpc-asm-preview)

;;; ---- Internal helpers ------------------------------------------------------

(defun simpc-asm-preview--build-command ()
  "Return the compiler invocation as a list of strings."
  (append
   (list simpc-asm-preview-compiler)
   simpc-asm-preview-flags
   (when simpc-asm-preview-intel-syntax '("-masm=intel"))
   ;; -S  → compile to assembly
   ;; -x c → treat stdin as C (needed when reading from a pipe)
   ;; -o - → write assembly to stdout
   '("-S" "-x" "c" "-o" "-" "-")))

(defun simpc-asm-preview--strip (asm)
  "Remove noisy directives and mangled labels from ASM string."
  (let ((lines (split-string asm "\n"))
        (kept  '()))
    (dolist (line lines)
      (unless (or (string-match-p "^\\s-*\\.\\w" line)    ; .cfi, .section, .type etc.
                  (string-match-p "^\\s-*#" line)         ; preprocessor noise
                  (string-match-p "^\"\\?\\?" line)       ; MSVC/clang mangled string literals
                  (string-match-p "^\\.LC[0-9]+:" line))  ; GCC/clang rodata labels
        (push line kept)))
    (string-join (nreverse kept) "\n")))

(defun simpc-asm-preview--run (source-string)
  "Compile SOURCE-STRING in memory and return the ASM output as a string.
Signals a `user-error' if compilation fails, showing the error output."
  (let* ((cmd      (simpc-asm-preview--build-command))
         (err-file (make-temp-file "simpc-asm-err"))
         (out-buf  (generate-new-buffer " *simpc-asm-out*"))
         exit-code asm err)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert source-string)
            (setq exit-code
                  (apply #'call-process-region
                         (point-min) (point-max)
                         (car cmd)
                         nil
                         (list out-buf err-file)
                         nil
                         (cdr cmd))))
          (setq asm (with-current-buffer out-buf (buffer-string)))
          (setq err (with-temp-buffer
                      (insert-file-contents err-file)
                      (buffer-string))))
      (kill-buffer out-buf)
      (delete-file err-file))
    (if (zerop exit-code)
        asm
      (user-error "Compiler error:\n%s" err))))

(defun simpc-asm-preview--display (asm-string source-desc)
  "Pop up or refresh the preview buffer with ASM-STRING.
Reuses an existing window if one is already showing the buffer,
or the other window in a 2-window frame, otherwise splits."
  (let ((cleaned (if simpc-asm-preview-strip-directives
                     (simpc-asm-preview--strip asm-string)
                   asm-string))
        (buf (get-buffer-create simpc-asm-preview-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "; Assembly for: %s\n" source-desc))
        (insert (format "; Compiler:     %s %s\n"
                        simpc-asm-preview-compiler
                        (string-join simpc-asm-preview-flags " ")))
        (insert (format "; Syntax:       %s\n\n"
                        (if simpc-asm-preview-intel-syntax "Intel" "AT&T")))
        (insert cleaned))
      (if (fboundp 'asm-mode) (asm-mode) (prog-mode))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    ;; Display logic: reuse existing window > other window in split > new split
    (let ((existing-win (get-buffer-window buf)))
      (cond
       ;; Already visible somewhere – just update it in place
       (existing-win
        (set-window-buffer existing-win buf))
       ;; Exactly 2 windows: put it in the other one without splitting again
       ((= (length (window-list)) 2)
        (set-window-buffer (next-window) buf))
       ;; No split yet: open a side window
       (t
        (display-buffer buf
                        '((display-buffer-in-side-window)
                          (side . right)
                          (window-width . 0.45)))))
      ;; Return point to top in whichever window is showing it
      (when-let ((win (get-buffer-window buf)))
        (set-window-point win (point-min))))))

;;; ---- Interactive commands --------------------------------------------------

;;;###autoload
(defun simpc-asm-preview-buffer ()
  "Compile the current buffer's C code and display the generated assembly.
No file is written; the source is piped directly to the compiler."
  (interactive)
  (let* ((source (buffer-string))
         (desc   (or (buffer-file-name) (buffer-name)))
         (asm    (simpc-asm-preview--run source)))
    (simpc-asm-preview--display asm desc)))

;;;###autoload
(defun simpc-asm-preview-region (beg end)
  "Compile the selected region's C code and display the assembly."
  (interactive "r")
  (let* ((source (buffer-substring-no-properties beg end))
         (asm    (simpc-asm-preview--run source)))
    (simpc-asm-preview--display asm "region")))

(defun simpc-asm-preview-close ()
  "Close the ASM preview side window."
  (interactive)
  (when-let ((win (get-buffer-window simpc-asm-preview-buffer-name)))
    (delete-window win)))

(defvar simpc-asm-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c a s") #'simpc-asm-preview-buffer)
    (define-key map (kbd "C-c a r") #'simpc-asm-preview-region)
    (define-key map (kbd "C-c a q") #'simpc-asm-preview-close)
    map)
  "Keymap for `simpc-asm-preview-mode'.")

;;;###autoload
(define-minor-mode simpc-asm-preview-mode
  "Minor mode that lets you preview compiler-generated assembly on the fly.

\\{simpc-asm-preview-mode-map}"
  :lighter " ASMprev"
  :keymap  simpc-asm-preview-mode-map
  :group   'simpc-asm-preview)

(provide 'simpc-asm-preview)
;;; simpc-asm-preview.el ends here
