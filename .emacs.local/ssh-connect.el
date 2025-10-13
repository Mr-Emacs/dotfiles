;;; ssh-connect.el --- SSH connection helper for Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; This script provides functions to connect to remote hosts via SSH
;; with prompts for hostname, username, and password.
;; It now hides IP addresses and uses hostnames/aliases in buffer names and Dired paths.

;;; Code:

(defun ssh--get-hostname ()
  "Prompt for hostname or IP, and if IP is entered, ask for an alias."
  (let ((input (read-string "Hostname/IP: ")))
    (if (string-match-p "^[0-9.]+$" input)
        (read-string (format "Alias for %s: " input))
      input)))

(defun ssh-connect ()
  "Connect to a remote host via SSH with prompts for credentials."
  (interactive)
  (let* ((input (read-string "Hostname/IP: "))
         (hostname (if (string-match-p "^[0-9.]+$" input)
                       (read-string (format "Alias for %s: " input))
                     input))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (remote-path (format "/ssh:%s@%s#%s:/" username hostname port)))
    
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Connecting to %s@%s:%s..." username hostname port)
    (dired remote-path)))

(defun ssh-connect-with-shell ()
  "Connect to a remote host via SSH and open a shell."
  (interactive)
  (let* ((input (read-string "Hostname/IP: "))
         (hostname (if (string-match-p "^[0-9.]+$" input)
                       (read-string (format "Alias for %s: " input))
                     input))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (buffer-name (format "*ssh-%s@%s*" username hostname)))
    
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening SSH shell to %s@%s:%s..." username hostname port)
    (let ((default-directory (format "/ssh:%s@%s#%s:/" username hostname port)))
      (shell buffer-name))))

(defun ssh-connect-terminal ()
  "Connect to a remote host via SSH using term mode."
  (interactive)
  (let* ((input (read-string "Hostname/IP: "))
         (hostname (if (string-match-p "^[0-9.]+$" input)
                       (read-string (format "Alias for %s: " input))
                     input))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (buffer-name (format "*ssh-term-%s@%s*" username hostname)))
    
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening SSH terminal to %s@%s:%s..." username hostname port)
    (let ((ssh-command (format "ssh -p %s %s@%s" port username hostname)))
      (term ssh-command)
      (rename-buffer buffer-name t))))

(defun ssh-edit-file ()
  "Edit a file on a remote host via SSH."
  (interactive)
  (let* ((input (read-string "Hostname/IP: "))
         (hostname (if (string-match-p "^[0-9.]+$" input)
                       (read-string (format "Alias for %s: " input))
                     input))
         (username (read-string "Username: "))
         (port (read-string "Port (default 22): " nil nil "22"))
         (filepath (read-string "Remote file path: " "/home/"))
         (remote-file (format "/ssh:%s@%s#%s:%s" username hostname port filepath)))
    
    (when (or (string-empty-p hostname) (string-empty-p username))
      (error "Hostname and username are required"))
    
    (message "Opening file %s on %s@%s:%s..." filepath username hostname port)
    (find-file remote-file)))

(defun ssh-quick-connect ()
  "Quick SSH connection using a predefined format."
  (interactive)
  (let* ((connection-string (read-string "Connection (user@host:port): "))
         (parts (split-string connection-string "[@ :]"))
         (username (nth 0 parts))
         (input (nth 1 parts))
         (hostname (if (and input (string-match-p "^[0-9.]+$" input))
                       (read-string (format "Alias for %s: " input))
                     input))
         (port (if (nth 2 parts) (nth 2 parts) "22"))
         (remote-path (format "/ssh:%s@%s#%s:/" username hostname port)))
    
    (when (< (length parts) 2)
      (error "Invalid format. Use: user@host or user@host:port"))
    
    (message "Connecting to %s@%s:%s..." username hostname port)
    (dired remote-path)))

(defun ssh-menu ()
  "SSH connection menu."
  (interactive)
  (let ((choice (completing-read "SSH Action: " 
                                '("Connect (dired)"
                                  "Connect (shell)"
                                  "Connect (terminal)"
                                  "Edit file"
                                  "Quick connect"))))
    (cond
     ((string= choice "Connect (dired)") (ssh-connect))
     ((string= choice "Connect (shell)") (ssh-connect-with-shell))
     ((string= choice "Connect (terminal)") (ssh-connect-terminal))
     ((string= choice "Edit file") (ssh-edit-file))
     ((string= choice "Quick connect") (ssh-quick-connect)))))

(provide 'ssh-connect)

;;; ssh-connect.el ends here
