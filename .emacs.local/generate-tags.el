;;; generate-tags.el --- Generate Emacs-compatible TAGS for C/C++ projects

;;; Commentary:
;; This script provides a function to generate a TAGS file using Universal Ctags.
;; It asks for the project root and runs ctags with proper options.

;;; Code:

(defun generate-project-tags (project-root)
  "Generate an Emacs-compatible TAGS file for a C/C++ project.
PROJECT-ROOT is the root directory of your project."
  (interactive "DProject root: ")
  (let ((default-directory (file-name-as-directory project-root))
        (ctags-cmd "ctags -R -e --languages=C,C++ --c++-kinds=+p --fields=+iaS --extras=+q ."))
    (message "Generating TAGS in %s ..." project-root)
    (shell-command ctags-cmd)
    (visit-tags-table (expand-file-name "TAGS" project-root))
    (message "TAGS generated successfully at %s/TAGS" project-root)))

;;; Usage:
;; M-x generate-project-tags
;; Then select your project root directory.

(provide 'generate-tags)
;;; generate-tags.el ends here
