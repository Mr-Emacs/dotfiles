(set-font-size 24)
(set-font "/usr/local/share/fonts/Iosevka-Regular.ttf")
(set-tab-width 4)
(toggle-hidden-files)

(defun build-project ()
  "Build the current project"
  (interactive)
  (compile "make"))

(defun replace-tabs-with-spaces ()
  "Replace all tabs with spaces"
  (interactive)
  (replace-all "\t" "    "))

(defun delete-trailing-whitespace ()
  "Delete trailing whitespace in buffer"
  (interactive)
  (replace-regex "[ \t]+$" ""))

(defun duplicate-line ()
  "Duplicate the current line below"
  (interactive)
  (let ((line-text (get-line-text))
        (current-line (get-cursor-line)))
    (end-of-line)
    (newline)
    (insert-text line-text)
    (goto-line current-line)))

(defun goto-first-line ()
  "Jump to the first line of the buffer"
  (interactive)
  (beginning-of-buffer))

(defun goto-last-line ()
  "Jump to the last line of the buffer"
  (interactive)
  (end-of-buffer))

(defun center-cursor ()
  "Print current cursor position info"
  (interactive)
  (message (concat "Line: " (number-to-string (line-number-at-pos)))))

(defun git-blame-line ()
  "Git blame for current line"
  (interactive)
  (let ((fname (buffer-file-name))
        (lnum (number-to-string (line-number-at-pos))))
    (if fname
        (compile (concat "git blame -L " lnum "," lnum " " fname))
      (message "No file"))))

(defun insert-date ()
  "Insert current date at cursor"
  (interactive)
  (let ((date (shell-command-to-string "date +%Y-%m-%d")))
    (insert date)))

(defun insert-timestamp ()
  "Insert full timestamp at cursor"
  (interactive)
  (let ((ts (shell-command-to-string "date '+%Y-%m-%d %H:%M:%S'")))
    (insert ts)))

(defun count-words ()
  "Count words in buffer"
  (interactive)
  (let ((result (shell-command-to-string
                 (concat "echo '" (get-buffer-text) "' | wc -w"))))
    (message (concat "Word count: " result))))

(defun count-lines-in-buffer ()
  "Show total line count"
  (interactive)
  (message (concat "Total lines: " (number-to-string (count-lines)))))

(defun insert-line-above ()
  "Insert a blank line above current line"
  (interactive)
  (beginning-of-line)
  (newline)
  (previous-line))

(defun insert-line-below ()
  "Insert a blank line below current line"
  (interactive)
  (end-of-line)
  (newline))

(defun kill-whole-line ()
  "Kill the entire current line including newline"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

(defun join-next-line ()
  "Join current line with the next line"
  (interactive)
  (end-of-line)
  (delete-char))

(defun comment-line-lisp ()
  "Comment current line with semicolon"
  (interactive)
  (beginning-of-line)
  (insert ";; "))

(defun comment-line-c ()
  "Comment current line with // "
  (interactive)
  (beginning-of-line)
  (insert "// "))

(defun comment-line-python ()
  "Comment current line with #"
  (interactive)
  (beginning-of-line)
  (insert "# "))

(defun comment-line-rust ()
  "Comment current line with // for Rust"
  (interactive)
  (beginning-of-line)
  (insert "// "))

(defun reload-config ()
  "Reload the falk config file"
  (interactive)
  (load-file "~/.falk.lisp")
  (message "Config reloaded!"))

(defun open-config ()
  "Open the falk config file for editing"
  (interactive)
  (find-file "~/.falk.lisp"))

(defun open-scratch ()
  "Switch to scratch buffer"
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun save-all ()
  "Save current buffer"
  (interactive)
  (save-buffer)
  (message "Saved!"))

(defun show-filename ()
  "Show current file path"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (message fname)
      (message "(no file)"))))

(defun make-file-executable ()
  "chmod +x the current file"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (progn
          (shell-command (concat "chmod +x " fname))
          (message (concat "Made executable: " fname)))
      (message "Buffer has no file"))))

(defun insert-c-header-guard ()
  "Insert a C header guard"
  (interactive)
  (let ((name (read-string "Header name (e.g. MYHEADER): ")))
    (insert "#ifndef " name "_H")
    (newline)
    (insert "#define " name "_H")
    (newline)
    (newline)
    (insert "#endif // " name "_H")))

(defun find-word ()
  "Interactive find"
  (interactive)
  (let ((pattern (read-string "Find: ")))
    (find pattern)))

(defun replace-word ()
  "Interactive find and replace"
  (interactive)
  (let ((pattern (read-string "Find: "))
        (replacement (read-string "Replace with: ")))
    (find pattern)
    (replace replacement)))

(defun replace-word-all ()
  "Interactive replace all"
  (interactive)
  (let ((pattern (read-string "Find: "))
        (replacement (read-string "Replace all with: ")))
    (replace-all pattern replacement)))

(defun which-program ()
  "Find a program in PATH"
  (interactive)
  (let ((name (read-string "Which program: ")))
    (message (shell-command-to-string (concat "which " name)))))

(defun strip-ansi-codes ()
  "Remove ANSI escape codes from buffer"
  (interactive)
  (replace-regex "\033\\[[0-9;]*m" "")
  (message "ANSI codes stripped"))

(defun normalize-whitespace ()
  "Replace multiple spaces with single space"
  (interactive)
  (replace-regex "  +" " ")
  (message "Whitespace normalized"))

(defun build-and-run ()
  "Build project then run it"
  (interactive)
  (compile "make && ./$(basename $(pwd))"))

(defun build-release ()
  "Build in release mode"
  (interactive)
  (compile "make release"))

(defun build-clean ()
  "Clean build artifacts"
  (interactive)
  (compile "make clean"))

(defun run-tests ()
  "Run project tests"
  (interactive)
  (compile "make test"))

(defun compile-c-file ()
  "Compile current C file directly"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (compile (concat "gcc -Wall -Wextra -g -o out " fname " -lm && echo 'OK'"))
      (message "No file to compile"))))

(defun compile-python-check ()
  "Syntax check current Python file"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (compile (concat "python3 -m py_compile " fname " && echo 'Syntax OK'"))
      (message "No file"))))

(defun run-python-file ()
  "Run current Python file"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (compile (concat "python3 " fname))
      (message "No file"))))

(defun run-shell-file ()
  "Run current file as shell script"
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (compile (concat "bash " fname))
      (message "No file"))))

(defun git-status ()
  "Show git status"
  (interactive)
  (compile "git status"))

(defun git-diff ()
  "Show git diff"
  (interactive)
  (compile "git diff"))

(defun git-log ()
  "Show git log (last 20 commits)"
  (interactive)
  (compile "git log --oneline -20"))

(defun git-add-all ()
  "Git add all changes"
  (interactive)
  (compile "git add -A && echo 'Staged all changes'"))

(defun git-push ()
  "Git push"
  (interactive)
  (compile "git push"))

(defun git-pull ()
  "Git pull"
  (interactive)
  (compile "git pull"))

(defun split-and-open-config ()
  "Split window and open config"
  (interactive)
  (split-window-vertically)
  (find-file "~/.falk.lisp"))

(defun close-other-windows ()
  "Close the non-active window"
  (interactive)
  (other-window)
  (delete-window))

(defun toggle-split-direction ()
  "Split horizontally"
  (interactive)
  (split-window-horizontally))

(defun insert-todo ()
  "Insert a TODO comment"
  (interactive)
  (insert "TODO: "))

(defun insert-fixme ()
  "Insert a FIXME comment"
  (interactive)
  (insert "FIXME: "))

(defun insert-separator ()
  "Insert a visual separator line"
  (interactive)
  (insert ";; ============================================================"))

(defun insert-c-main ()
  "Insert a C main function template"
  (interactive)
  (insert "#include <stdio.h>")
  (newline)
  (insert "#include <stdlib.h>")
  (newline)
  (newline)
  (insert "int main(int argc, char** argv) {")
  (newline)
  (insert "    ")
  (newline)
  (insert "    return 0;")
  (newline)
  (insert "}"))

(defun insert-python-main ()
  "Insert Python main template"
  (interactive)
  (insert "def main():")
  (newline)
  (insert "    pass")
  (newline)
  (newline)
  (insert "if __name__ == '__main__':")
  (newline)
  (insert "    main()"))

(defun insert-shebang-bash ()
  "Insert bash shebang at top"
  (interactive)
  (beginning-of-buffer)
  (insert "#!/usr/bin/env bash")
  (newline)
  (insert "set -euo pipefail")
  (newline)
  (newline))

(defun insert-shebang-python ()
  "Insert python shebang at top"
  (interactive)
  (beginning-of-buffer)
  (insert "#!/usr/bin/env python3")
  (newline)
  (newline))

(defun font-small ()
  "Set font to small size"
  (interactive)
  (set-font-size 14))

(defun font-medium ()
  "Set font to medium size"
  (interactive)
  (set-font-size 20))

(defun font-large ()
  "Set font to large size"
  (interactive)
  (set-font-size 28))

(defun font-huge ()
  "Set font to huge (presentation) size"
  (interactive)
  (set-font-size 40))

(message "Falk config loaded!")
