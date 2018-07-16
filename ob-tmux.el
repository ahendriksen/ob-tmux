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

(defvar org-babel-tmux-default-window-name "ob1"
  "The default tmux window name used for windows that are not
explicitly named in an org session.")

(defvar org-babel-default-header-args:tmux
  '((:results . "silent")
    (:session . "default")
    (:terminal . "gnome-terminal"))
  "Default arguments to use when running tmux source blocks.")

(add-to-list 'org-src-lang-modes '("tmux" . sh))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-babel interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-babel-execute:tmux (body params)
  "Send a block of code via tmux to a terminal using Babel.
\"default\" session is used when none is specified."
  (message "Sending source code block to interactive terminal session...")
  (save-window-excursion
    (let* ((org-session (cdr (assq :session params)))
	   (terminal (cdr (assq :terminal params)))
	   (socket (cdr (assq :socket params)))
	   (ob-session (ob-tmux--from-org-session org-session socket))
           (session-alive (ob-tmux--session-alive-p ob-session))
	   (window-alive (ob-tmux--window-alive-p ob-session)))
      ;; Create tmux session and window if they do not yet exist
      (unless session-alive (ob-tmux--create-session ob-session))
      (unless window-alive (ob-tmux--create-window ob-session))
      ;; Start terminal window if the session does not yet exist
      (unless session-alive
	(ob-tmux--start-terminal-window ob-session terminal))
      ;; Wait until tmux window is available
      (while (not (ob-tmux--window-alive-p ob-session)))
      ;; Disable window renaming from within tmux
      (ob-tmux--disable-renaming ob-session)
      (ob-tmux--send-body
       ob-session (org-babel-expand-body:generic body params)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ob-tmux object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-defstruct (ob-tmux- (:constructor ob-tmux--create)
			(:copier ob-tmux--copy))
  session
  window
  socket)

(defun ob-tmux--from-org-session (org-session &optional socket)
  "Creates a new ob-tmux-session object from org-session specification."
  (defun -tmux-session (org-session)
    (let* ((session (car (split-string org-session ":"))))
      (concat org-babel-tmux-session-prefix
	      (if (string-empty-p session) "default" session))))
  (defun -tmux-window (org-session)
    (let* ((window (cadr (split-string org-session ":"))))
      (if (string-empty-p window) nil window)))

  (ob-tmux--create
   :session (-tmux-session org-session)
   :window (-tmux-window org-session)
   :socket socket))

(defun ob-tmux--window-default (ob-session)
  "Extracts the tmux window from the ob-tmux- object.
Returns `org-babel-tmux-default-window-name' if no window specified."
  (if (ob-tmux--window ob-session)
      (ob-tmux--window ob-session)
      org-babel-tmux-default-window-name))

(defun ob-tmux--target (ob-session)
  "Constructs a tmux target from the `ob-tmux-' object.

If no window is specified, use first window."
  (let* ((target-session (ob-tmux--session ob-session))
	 (window (ob-tmux--window ob-session))
	 (target-window (if window (concat "=" window) "^")))
    (concat target-session ":" target-window)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process execution functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--execute (&rest args)
  "Executes a tmux command with arguments as given."
  (apply 'start-process
	 "ob-tmux" "*Messages*" org-babel-tmux-location args))

(defun ob-tmux--execute-string (&rest args)
  "Executes a tmux command with arguments as given.
Returns stdout as a string."
  (shell-command-to-string
   (concat org-babel-tmux-location " "
	   (s-join " " args))))

(defun ob-tmux--start-terminal-window (ob-session terminal)
  "Starts a terminal window with tmux attached to session."
  (let* ((process-name (concat "org-babel: terminal")))
    (if (string-equal terminal "xterm")
	(start-process process-name "*Messages*"
		       terminal
		       "-T" (ob-tmux--target ob-session)
		       "-e" org-babel-tmux-location "attach-session"
		       "-t" (ob-tmux--target ob-session))
      (start-process process-name "*Messages*"
		     terminal "--"
		     org-babel-tmux-location "attach-session"
		     "-t" (ob-tmux--target ob-session)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--create-session (ob-session)
  "Creates a tmux session if it does not yet exist."
  (unless (ob-tmux--session-alive-p ob-session)
    (ob-tmux--execute
     ;; TODO: set socket
     "new-session"
     "-d" ;; just create the session, don't attach.
     "-c" (expand-file-name "~") ;; start in home directory
     "-s" (ob-tmux--session ob-session)
     "-n" (ob-tmux--window-default ob-session))))

(defun ob-tmux--create-window (ob-session)
  "Creates a tmux window in session if it does not yet exist."
  (unless (ob-tmux--window-alive-p ob-session)
    (ob-tmux--execute
     ;; TODO: set socket
     "new-window"
     "-c" (expand-file-name "~") ;; start in home directory
     "-n" (ob-tmux--window-default ob-session)
     "-t" (ob-tmux--session ob-session))))

(defun ob-tmux--set-window-option (ob-session option value)
  "If window exists, set option for window."
  (when (ob-tmux--window-alive-p ob-session)
    (ob-tmux--execute
     ;; TODO set socket
     "set-window-option"
     "-t" (ob-tmux--target ob-session)
     option value)))

(defun ob-tmux--disable-renaming (ob-session)
  "Disable renaming features for tmux window.

Disabling renaming improves the chances that ob-tmux will be able
to find the window again later."
  (progn
    (ob-tmux--set-window-option ob-session "allow-rename" "off")
    (ob-tmux--set-window-option ob-session "automatic-rename" "off")))

(defun ob-tmux--send-keys (ob-session line)
  "If window exists, send a line of text to it."
  (when (ob-tmux--window-alive-p ob-session)
    (ob-tmux--execute
     ;; TODO set socket
     "send-keys"
     "-l"
     "-t" (ob-tmux--target ob-session)
     line "\n")))

(defun ob-tmux--send-body (ob-session body)
  "If window exists, send body to it."
  (let ((lines (split-string body "[\n\r]+")))
    (when (ob-tmux--window-alive-p ob-session)
      (mapc (lambda (l) (ob-tmux--send-keys ob-session l)) lines))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmux interrogation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--session-alive-p (ob-session)
  "Check if SESSION exists by parsing output of \"tmux ls\"."
  (let* ((tmux-ls (ob-tmux--execute-string "ls -F '#S'"))
	 (tmux-session (ob-tmux--session ob-session)))
    (car
     (seq-filter (lambda (x) (string-equal tmux-session x))
		 (split-string tmux-ls "\n")))))

(defun ob-tmux--window-alive-p (ob-session)
  "Check if WINDOW exists in tmux session.

If no window is specified in org-session, returns 't."
  (let* ((window (ob-tmux--window ob-session))
	 (target (ob-tmux--target ob-session))
	 (output (ob-tmux--execute-string
		  "list-panes"
		  "-F 'yes_exists'"
		  "-t" (concat "'" target "'"))))
    (cond (window
	   (string-equal "yes_exists\n" output))
	  ((null window)
	   't))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ob-tmux--open-file (path)
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-substring (point-min) (point-max))))

(defun ob-tmux--test ()
  "Test if the default setup works. The terminal should shortly flicker."
  (interactive)
  (let* ((random-string (format "%s" (random 99999)))
         (tmpfile (org-babel-temp-file "ob-tmux-test-"))
         (body (concat "echo '" random-string "' > " tmpfile))
         tmp-string)
    (org-babel-execute:tmux body org-babel-default-header-args:tmux)
    ;; XXX: need to find a better way to do the following
    (while (or (not (file-readable-p tmpfile))
	       (= 0 (length (ob-tmux--open-file tmpfile))))
      ;; do something, otherwise this will be optimized away
      (format "org-babel-tmux: File not readable yet."))
    (setq tmp-string (ob-tmux--open-file tmpfile))
    (delete-file tmpfile)
    (message (concat "org-babel-tmux: Setup "
                     (if (string-match random-string tmp-string)
                         "WORKS."
		       "DOESN'T work.")))))

(provide 'ob-tmux)



;;; ob-tmux.el ends here
