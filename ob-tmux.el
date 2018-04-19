;;; ob-tmux.el --- Babel Support for Interactive Terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.
;; Copyright (C) 2017 Allard Hendriksen

;; Author: Benjamin Andresen
;; Keywords: literate programming, interactive shell
;; Homepage: http://orgmode.org

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for interactive terminals.  Mostly shell scripts.
;; Heavily inspired by 'eev' from Eduardo Ochs
;;
;; Adding :terminal as header arguments
;; :terminal must support the -T (title) and -e (command) parameter
;;           or allow for the command to be added after '--'
;;
;; You can test the default setup. (gnome-terminal) with
;; M-x org-babel-tmux-test RET

;;; Code:
(require 'ob)
(require 'seq)

(defvar org-babel-tmux-location "tmux"
  "The command location for tmux.
In case you want to use a different tmux than one selected by your $PATH")

(defvar org-babel-tmux-session-prefix "org-babel-session-"
  "The string that will be prefixed to tmux sessions started by ob-tmux")

(defvar org-babel-default-header-args:tmux
  '((:results . "silent")
    (:session . "default")
    (:terminal . "gnome-terminal"))
  "Default arguments to use when running tmux source blocks.")

(add-to-list 'org-src-lang-modes '("tmux" . sh))

(defun org-babel-execute:tmux (body params)
  "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((session (cdr (assq :session params)))
           (session-alive (org-babel-tmux-session-alive-p session))
	   (window-alive (org-babel-tmux-window-alive-p session)))
      ;; Prepare session unless both the tmux session and window exist.
      (unless (and session-alive window-alive)
	(org-babel-prep-session:tmux session params))
      (org-babel-tmux-session-execute-string
       session (org-babel-expand-body:generic body params)))))

(defun org-babel-prep-session:tmux (_session params)
  "Prepare SESSION according to the header arguments specified in
PARAMS. Starts a terminal window if the tmux session does not yet
exist. No terminal window is started, if the only tmux window
must be created."
  (let* ((session (cdr (assq :session params)))
         (terminal (cdr (assq :terminal params)))
	 (process-name (concat "org-babel: terminal (" session ")"))
	 (session-alive (org-babel-tmux-session-alive-p session))
	 (window-alive (org-babel-tmux-window-alive-p session)))

    ;; First create tmux session and windows
    (unless session-alive (org-babel-tmux-create-session session))
    (unless window-alive (org-babel-tmux-create-window session))
    (unless session-alive
      (start-process process-name "*Messages*"
		     terminal "--"
		     org-babel-tmux-location "attach-session"
		     "-t" (org-babel-tmux-target-session session)))
    ;; XXX: Is there a better way than the following?
    ;; wait until tmux session is available before returning
    (while (not (org-babel-tmux-session-alive-p session)))))

;; helper functions

(defun org-babel-tmux-start-terminal-window (session terminal)
  "Starts a terminal window with tmux attached to session."
  (let* ((process-name (concat "org-babel: terminal (" session ")")))
    (if (string-equal terminal "xterm")
	(start-process process-name "*Messages*"
		       terminal
		       "-T" (org-babel-tmux-target-session session)
		       "-e" org-babel-tmux-location "attach-session"
		       "-t" (org-babel-tmux-target-session session))
      (start-process process-name "*Messages*"
		     terminal "--"
		     org-babel-tmux-location "attach-session"
		     "-t" (org-babel-tmux-target-session session)))))

(defun org-babel-tmux-create-session (session)
  "Creates a tmux session if it does not yet exist."
  (unless (org-babel-tmux-session-alive-p session)
    (start-process "tmux-create-session" "*Messages*"
		   org-babel-tmux-location "new-session"
		   "-d" ;; just create the session, don't attach.
		   "-c" (expand-file-name "~/") ;; start in home directory
		   "-s" (org-babel-tmux-session session)
		   "-n" (org-babel-tmux-window-default session))))

(defun org-babel-tmux-create-window (session)
  "Creates a tmux window in session if it does not yet exist."
  (unless (org-babel-tmux-window-alive-p session)
    (start-process "tmux-create-window" "*Messages*"
		   org-babel-tmux-location "new-window"
		   "-c" (expand-file-name "~/") ;; start in home directory
		   "-n" (org-babel-tmux-window-default session)
		   "-t" (org-babel-tmux-session session))))

(defun org-babel-tmux-send-keys (session line)
  "If SESSION exists, send a line of text to it."
  (let ((alive (org-babel-tmux-session-alive-p session)))
    (when alive
      (start-process "tmux-send-keys" "*Messages*"
	       "tmux" "send-keys"
	       "-t" (org-babel-tmux-target-session session)
	       line
	       "Enter"))))

(defun org-babel-tmux-session-execute-string (session body)
  "If SESSION exists, send BODY to it."
  (let ((alive (org-babel-tmux-session-alive-p session)))
    (when alive
      (let ((lines (split-string body "[\n\r]+")))
	(mapc (lambda (l) (org-babel-tmux-send-keys session l))
	      lines)))))

(defun org-babel-tmux-session (org-session)
  "Extracts the tmux session from the org session string."
  (concat org-babel-tmux-session-prefix
	  (car (split-string org-session ":"))))

(defun org-babel-tmux-window (org-session)
  "Extracts the tmux window from the org session string.
Can return nil if no window specified."
  (cadr (split-string org-session ":")))

(defun org-babel-tmux-window-default (org-session)
  "Extracts the tmux window from the org session string.
Returns '1' if no window specified."
  (let* ((tmux-window (cadr (split-string org-session ":"))))
    (if tmux-window tmux-window "1")))

(defun org-babel-tmux-target-session (org-session)
  "Constructs a target-session from the org session string."
  (concat (org-babel-tmux-session org-session)
	  ":"
	  (org-babel-tmux-window-default org-session)))

(defun org-babel-tmux-session-alive-p (session)
  "Check if SESSION exists by parsing output of \"tmux ls\"."
  (let* ((tmux-ls (shell-command-to-string "tmux ls -F '#S'"))
	 (tmux-session (org-babel-tmux-session session)))
    (car
     (seq-filter (lambda (x) (string-equal tmux-session x))
		 (split-string tmux-ls "\n")))))

(defun org-babel-tmux-window-alive-p (session)
  "Check if WINDOW exists in tmux session."
  (let* ((tmux-session (org-babel-tmux-session session))
	 (tmux-window (org-babel-tmux-window session))
	 (tmux-lws (shell-command-to-string
		   (concat "tmux list-windows -F '#W' -t '"
			   tmux-session "'"))))
    (if tmux-window
	(car
	 (seq-filter (lambda(x) (string-equal tmux-window x))
		     (split-string tmux-lws "\n")))
      't)))

(defun org-babel-tmux-open-file (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring (point-min) (point-max))))

(defun org-babel-tmux-test ()
  "Test if the default setup works. The terminal should shortly flicker."
  (interactive)
  (let* ((random-string (format "%s" (random 99999)))
         (tmpfile (org-babel-temp-file "ob-screen-test-"))
         (body (concat "echo '" random-string "' > " tmpfile " ; exit"))
         tmp-string)
    (org-babel-execute:tmux body org-babel-default-header-args:tmux)
    ;; XXX: need to find a better way to do the following
    (while (or (not (file-readable-p tmpfile))
	       (= 0 (length (org-babel-tmux-open-file tmpfile))))
      ;; do something, otherwise this will be optimized away
      (format "org-babel-screen: File not readable yet."))
    (setq tmp-string (org-babel-tmux-open-file tmpfile))
    (delete-file tmpfile)
    (message (concat "org-babel-screen: Setup "
                     (if (string-match random-string tmp-string)
                         "WORKS."
		       "DOESN'T work.")))))

(provide 'ob-tmux)



;;; ob-tmux.el ends here
