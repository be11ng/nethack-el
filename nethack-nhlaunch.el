;;; nethack-nhlaunch.el --- Negotiate with nhlaunch to start a game of nethack

;; Copyright (C) 2004  Free Software Foundation, Inc.

;; Author: Shawn Betts <katia_dilkina@verizon.net>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

(require 'nethack)

(defvar nh-network-user nil
  "The user to login as")

(defvar nh-network-password nil
  "The password to use")

(defvar nh-network-game nil
  "The game to play")

(defun nh-network-filter (proc str)
  (with-current-buffer (process-buffer proc)
    (insert str))
  (cond ((or (string-equal str "Welcome to the nethack-el server.\n")
	     (string-equal str (format "User %s added successfully.\n" nh-network-user)))
	 (process-send-string proc (format "login %s %s\n" nh-network-user nh-network-password)))
	((string-equal str (format "Welcome back %s.\n" nh-network-user))
	 (message "Starting nethack...")
	 (process-send-string proc (format "play %s\n" nh-network-game))
	 (with-current-buffer  (process-buffer proc)
	   (erase-buffer))
	 (nethack-start proc))
	((or (string-equal str (format "Failed to login %s.\n" nh-network-user))
	     (string-equal str "Error parsing name and password.\n"))
	 (delete-process proc)
	 (message str))
	((string-equal str (format "Unknown user %s.\n" nh-network-user))
	 (process-send-string proc (format "new %s %s\n" nh-network-user nh-network-password)))))

;;;###autoload
(defun nethack-connect-to-server (server port user passwd game)
  (interactive "sServer: \nsPort: \nsUser: \nsPasswd: \nsGame: ")
  (if (nethack-is-running)
	(message "Nethack process already running...")
    (if (get-buffer nh-proc-buffer-name)
	(kill-buffer nh-proc-buffer-name))
    (let ((proc (open-network-stream "nh" nh-proc-buffer-name server port)))
      (setq nh-network-user user)
      (setq nh-network-password passwd)
      (setq nh-network-game game)
      (set-process-filter proc 'nh-network-filter))))
  
(provide 'nethack-nhlaunch)
;;; nethack-nhlaunch.el ends here
