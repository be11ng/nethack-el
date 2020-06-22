;;; nethack.el --- run Nethack as a subprocess -*- lexical-binding:t -*-

;; Copyright (C) 2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske <rcyeske@vcn.bc.ca>
;; Created: Sat Mar 18 11:31:52 2000
;; Version: $Id$
;; Keywords: games

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
;; Note: This package requires external libraries and works currently
;; only on GNU/Linux systems.
;;
;; Note: If you ever update it, you need to restart Emacs afterwards.
;;
;; To activate the package put
;;
;; (nethack-install)
;;
;; somewhere in your .emacs.el .

;;; Code:

(require 'nethack-compat)
(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)
(require 'url)

(defgroup nethack nil
  "Emacs lisp frontend to the lisp window port of Nethack 3.4.0."
  :group 'games)

(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-highlight-delay 5
  "The number of turns to keep a changed status field highlighted."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-status-buffer-format
  "n w s d c i W C A\nL l g h p a e t f"
  "Format string for the status in `nh-status-buffer'."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-mode-line-format
  "s d c i W C g h p a e t"
  "Format string for the status on the mode-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-header-line-format
  "n w <L,l> A   f"
  "Format string for the status on the header-line."
  :type '(string)
  :group 'nethack)

(defcustom nethack-status-style t
  "Decides how the status will be displayed. Valid values are :map, :header-line, :mode-line, or t."
  :type '(symbol)
  :options '(:map :mode-line :header-line t)
  :group 'nethack)

(defcustom nethack-purge-buffers t
  "When this variable is non-nil, kill all nethack buffers when nethack quits."
  :type '(boolean)
  :group 'nethack)

;;; Insert variables that control how the status gets displayed here.

(defcustom nethack-use-tiles nil
  "If non-nil, use XPMs to draw tiles."
  :type '(boolean)
  :group 'nethack)

(defcustom nethack-map-mode-hook nil
  "Functions to be called after setting up the Nethack map."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-menu-mode-hook nil
  "Functions to be called after setting up a Nethack menu."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-before-print-message-hook nil
  "Hook run before a message is printed."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-message-style t
  "Decides where messages appear. :map means messages display in
the map buffer. t means in a seperate buffer."
  :type '(symbol)
  :options '(:map t)
  :group 'nethack)

(defcustom nethack-prompt-style t
  "Decides where nethack-el prompts for input. :map means in the
map buffer. t means in the minibuffer."
  :type '(symbol)
  :options '(:map t)
  :group 'nethack)

(defcustom nethack-end-hook nil
  "Hook run when nethack has ended."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-status-attribute-change-functions nil
  "List of functions to call after a status attribute change.

Three arguments are passed to each function: the name of the
attribute, the new value and the old value."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-load-hook nil
  "Hook run after loading nethack."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-add-menu-hook nil
  "Hook run after a menu option has been added."
  :type '(hook)
  :group 'nethack)


(defgroup nethack-faces nil
  "Customizations for faces used by Enethack."
  :group 'nethack)

(defface nethack-status-good-face
  `((((type tty)
      (class color))
     (:background "green" :foreground "black"))
    (((class color)
      (background light))
     (:background "darkseagreen2"))
    (((class color)
      (background dark))
     (:background "green4")))
  "Face for highlighting good changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-status-bad-face
  `((((type tty)
      (class color))
     (:background "red"))
    (((class color)
      (background light))
     (:background "pink"))
    (((class color)
      (background dark))
     (:background "red"))
    (t
     (:inverse-video t)))
  "Face for highlighting bad changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-status-neutral-face
  `((((type tty)
      (class color))
     (:foreground "white" :background "blue"))
    (((type tty)
      (class mono))
     (:inverse-video t))
    (((class color)
      (background dark))
     (:background "blue3"))
    (((class color)
      (background light))
     (:background "lightgoldenrod2"))
    (t
     (:background "gray")))
  "Face for highlighting neutral changes in the status buffer."
  :group 'nethack-faces)

(defface nethack-message-highlight-face
  '((t (:foreground "black" :background "green")))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defface nethack-atr-none-face
  `((t ()))
  "Nethack default face."
  :group 'nethack-faces)

(defface nethack-atr-uline-face
  `((t (:underline t)))
  "Nethack underline face.")

(defface nethack-atr-bold-face
  `((t (:bold t)))
  "Nethack bold face."
  :group 'nethack-faces)

(defface nethack-atr-blink-face
  `((t (:inverse-video t)))
  "Nethack blink face."
  :group 'nethack-faces)

(defface nethack-atr-inverse-face
  `((t (:inverse-video t)))
  "Nethack inverse face."
  :group 'nethack-faces)

(defface nethack-black-face
  `((t (:foreground "dark blue")))
  "nethack black face"
  :group 'nethack-faces)

(defface nethack-red-face
  `((((type tty) (class color))
     (:foreground "red"))
    (((class color))
     (:foreground "red"))
    (t (:foreground "gray")))
  "nethack red"
  :group 'nethack-faces)

(defface nethack-green-face
  `((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "lime green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "nethack green"
  :group 'nethack-faces)

(defface nethack-brown-face
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color) (background dark))
     (:foreground "chocolate"))
    (((class color) (background light))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "nethack brown"
  :group 'nethack-faces)

(defface nethack-blue-face
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color) (background dark))
     (:foreground "dark blue"))
    (((class color) (background light))
     (:foreground "dark blue"))
    (t (:foreground "gray")))
  "nethack blue"
  :group 'nethack-faces)

(defface nethack-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color) (background dark))
     (:foreground "dark magenta"))
    (((class color) (background light))
     (:foreground "dark magenta"))
    (t (:foreground "gray")))
  "nethack magenta"
  :group 'nethack-faces)

(defface nethack-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "dark cyan"))
    (((class color) (background light))
     (:foreground "cyan4"))
    (t (:foreground "gray")))
  "nethack cyan"
  :group 'nethack-faces)

(defface nethack-gray-face
  `((((type tty) (class color))
     (:foreground "white"))
    (((class color) (background dark))
     (:foreground "lightgray"))
    (((class color) (background light))
     (:foreground "darkgray"))
    (t (:foreground "gray")))
  "nethack gray"
  :group 'nethack-faces)

(defface nethack-dark-gray-face
  `((((type tty) (class color))
     (:foreground "black" :bold t))
    (((class color) (background dark))
     (:foreground "darkgray"))
    (((class color) (background light))
     (:foreground "lightgray"))
    (t (:foreground "gray")))
  "nethack dark gray"
  :group 'nethack-faces)

(defface nethack-orange-face
  `((((type tty) (class color))
     (:foreground "red" :bold t))
    (((class color))
     (:foreground "orange"))
    (t (:foreground "gray")))
  "nethack light orange"
  :group 'nethack-faces)

(defface nethack-bright-green-face
  `((((type tty) (class color))
     (:foreground "green" :bold t))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t (:foreground "gray")))
  "nethack bright green"
  :group 'nethack-faces)

(defface nethack-yellow-face
  `((((type tty) (class color))
     (:foreground "yellow" :bold t))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow3"))
    (t (:foreground "gray")))
  "nethack yellow"
  :group 'nethack-faces)

(defface nethack-bright-blue-face
  `((((type tty) (class color))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "blue"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "nethack bright blue"
  :group 'nethack-faces)

(defface nethack-bright-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta" :bold t))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack bright magenta"
  :group 'nethack-faces)

(defface nethack-bright-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan" :bold t))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan3"))
    (t (:foreground "gray")))
  "nethack bright cyan"
  :group 'nethack-faces)

(defface nethack-white-face
  `((((type tty) (class color))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:foreground "gray")))
  "nethack white"
  :group 'nethack-faces)

(defface nethack-map-tile-face
  `((((type tty))
     nil)
    (t (:height 16)))
  "Map face with height less than the tile size (16 pixels)."
  :group 'nethack-faces)

(defface nethack-pet-face
  `((((type tty) (class color))
     (:foreground "black" :background "white" :bold t))
    (((class color) (background dark))
     (:foreground "black" :background "white"))
    (((class color) (background light))
     (:foreground "white" :background "black"))
    (t (:foreground "gray")))
  "nethack white"
  :group 'nethack-faces)



;;; Installation

;; Much of this code was adapted from pdf-tools, since both of these
;; need to call an external program to do the heavy lifting, and that
;; program needs to be built from source.

(defconst nethack-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "The directory from where this library was first loaded.")

(defcustom nethack-program
  (expand-file-name "build/nethack" nethack-directory)
  "Program to run to start a game of Nethack.

You can influence the location of the build directory by setting
this variable (eventually, not yet implemented)."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program-args nil
  "Arguments to pass to `nethack-program'."
  :type '(repeat string)
  :group 'nethack)

(defcustom nethack-version
  "3.6.6"
  "The NetHack version to download, install, and bulid."
  :group 'nethack
  :type 'string)

;; It might be a bad, bad practice to make these functions, but it made sense at
;; the time.
(defun nethack-version-nodots ()
  "The NetHack version without separating dots."
  (replace-regexp-in-string "\\." "" nethack-version))

(defun nethack-query-for-version ()
  "Queries the user for the NetHack version.

Currently, the two supported versions are 3.6.6 and 3.4.3."
  (interactive)
  (read-answer "NetHack version "
               '(("3.6.6" ?6 "366")
                 ("3.4.3" ?3 "343"))))

(defun nethack-installed-p ()
  "Determine if a patched NetHack is installed.

Checks whether a NetHack executable exists, and if running it
results in an output with prefix ``(nhapi-raw-print''."
  (and nethack-program
       (string-prefix-p
        "(nhapi-raw-print"
        (shell-command-to-string (concat nethack-program " --version")))))

(defun nethack-download-nethack ()
  "Download the nethack source from nethack.org."
  (let* ((nethack-tar (concat "/nethack-" (nethack-version-nodots) "-src.tgz"))
         (nethack-url
          (concat "https://nethack.org/download/" nethack-version nethack-tar)))
    (url-copy-file nethack-url (expand-file-name "build/nethack.tgz"
                                                 nethack-directory)
                   t)))                 ; It's OK if it already exists.

(defun nethack-untar-nethack (build-directory)
  "Untar the nethack source out of nethack-tar.

Untars the file nethack.tgz located in BUILD-DIRECTORY into
BUILD-DIRECTORY/nethack-src.

Note that this is system specific to GNU tar and BSD tar, since
it relies on using the flag --strip-components."
  (let ((source-directory (expand-file-name "nethack-src" build-directory)))
    (unless (file-exists-p source-directory)
      (mkdir source-directory))
    (shell-command
     (format "tar xzf %s/nethack.tgz -C %s %s"
             build-directory
             source-directory
             "--strip-components=1 --ignore-command-error"))))

(defun nethack-build-program (target-directory
                              &optional
                              callback
                              build-directory)
  "Build the NetHack program in the background.

Install into TARGET-DIRECTORY, which should be a directory.

If CALLBACK is non-nil, it should be a function.  It is called
with the compiled executable as the single argument or nil, if
the build failed.

Expect sources to be in BUILD-DIRECTORY.  If nil, expect it to be
in `nethack-directory'.

Returns the buffer of the compilation process."
  (unless callback (setq callback #'ignore))
  (unless build-directory
    (setq build-directory (expand-file-name "build" nethack-directory)))
  (cl-check-type target-directory file-directory)
  (setq target-directory (file-name-as-directory
                          (expand-file-name target-directory)))
  (cl-check-type build-directory (and (not null) file-directory))
  (nethack-untar-nethack build-directory)
  (let* ((compilation-cmd
          (format
           "%s%s make -C %s %s && make -C %s %s%s && %s%s make -C %s %s"
           "NH_VER_NODOTS=" (nethack-version-nodots)
           build-directory "patch"
           build-directory "hints"
           (if (string-equal "36" (substring (nethack-version-nodots)
                                             nil -1))
               "-3.6"                   ; install the linux-lisp hints for >3.6
             "")
           "PREFIX=" target-directory
           build-directory "build"))
         (compilation-buffer
          (compilation-start compilation-cmd t) ; Use compilation-shell-minor-mode
          ))
    (if (get-buffer-window compilation-buffer)
        (select-window (get-buffer-window compilation-buffer))
      (pop-to-buffer compilation-buffer))
    (with-current-buffer compilation-buffer
      (setq-local compilation-error-regexp-alist nil)
      (add-hook 'compilation-finish-functions
                (lambda (_buffer status)
                  (funcall callback
                           (and (equal status "finished\n")
                                executable)))
                nil t)                  ; Locally add-hook
      (current-buffer))))


;;; Initialization

;;;###autoload
(defun nethack-install (&optional no-query-p
                                  no-download-p
                                  no-error-p)
  "Download, install, and patch nethack.

If the `nethack-program' is not running or does not appear to be
working, attempt to rebuild it.  If this build succeeded,
continue with the activation of the package.  Otherwise fail
silently, i.e. no error is is signaled.

Build the program (if necessary) without asking first, if
NO-QUERY-P is non-nil.

Do not download (but do untar) if NO-DOWNLOAD-P is non-nill.

Do not signal an error in case the build failed, if NO-ERROR-P is
non-nil."
  (interactive)
  (if (not (nethack-installed-p))
      (let ((target-directory
             (or (and (stringp nethack-program)
                      (file-name-directory nethack-program))
                 nethack-directory)))
        (if (or no-query-p
                (y-or-n-p "Need to (re)build the NetHack program, do it now?"))
            (progn
              (setq-default nethack-version
                            (or (and no-query-p "3.6.6")
                                (nethack-query-for-version)))
              (unless no-download-p (nethack-download-nethack))
              (nethack-build-program
               target-directory
               (lambda (executable) ; TODO: Do we need this?
                 (let ((msg (format
                             "Bulding the NetHack program %s"
                             (if executable "succeeded" "failed"))))
                   (if (not executable)
                       (funcall (if no-error-p #'message #'error) "%s" msg)
                     (message "%s" msg)
                     (setq-default nethack-program executable))))))
          (message "NetHack not activated")))))


;;; Process
(defvar nh-proc nil)
(defvar nh-proc-buffer-name "*nh-output*")
(defvar nh-proc-kill-buffer-on-quit t
  "When the process ends kill the process buffer if this is t.")
(defvar nh-log-buffer "*nh-log*")

;;;###autoload
(defun nethack ()
  "Start a game of Nethack.

The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (nethack-is-running)
      (progn
        (message "Nethack process already running...")
        (nhapi-restore-window-configuration))
    (progn
      ;; Start the process.
      (if (get-buffer nh-proc-buffer-name)
          (kill-buffer nh-proc-buffer-name))
      (nethack-start (apply 'start-process "nh" nh-proc-buffer-name
                            nethack-program nethack-program-args)))))

(defun nethack-is-running ()
  "return T if nethack is already running."
  (and (processp nh-proc)
       (member (process-status nh-proc) '(open run))))

(defun nethack-start (process)
  "Given the process, start nethack. Assumes nethack is not already running."
  (save-excursion
    (setq nh-proc process)
    (nh-reset-status-variables)
    (set-process-filter nh-proc 'nh-filter)
    (set-process-sentinel nh-proc 'nh-sentinel)))

(defun nethack-toggle-tiles ()
  "Toggle the use of tiles on the map."
  (interactive)
  (setq nethack-use-tiles (not nethack-use-tiles))
  (nethack-command-redraw-screen 2))

;;;; Process code to communicate with the Nethack executable
(defconst nh-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

(defun nh-sentinel (proc msg)
  "Nethack background process sentinel.
PROC is the process object and MSG is the exit message."
  (with-current-buffer (process-buffer proc)
    (nh-log (buffer-substring (point-min) (point)))
    (eval-region (point-min) (point-max))
    (insert "Nethack " msg)
    ;; (if (not (string-equal msg "Nethack finished"))
    ;;     (pop-to-buffer (current-buffer)))
    )
  (delete-process proc)
  (if nh-proc-kill-buffer-on-quit
      (kill-buffer (get-buffer nh-proc-buffer-name)))
  (if nethack-purge-buffers
      (nethack-kill-buffers))
  (let ((raw-print-buffer (get-buffer nh-raw-print-buffer-name)))
    (when raw-print-buffer
      (pop-to-buffer raw-print-buffer))))

(defvar nh-log-process-text t)
(defun nh-log (string)
  (if nh-log-process-text
      (with-current-buffer (get-buffer-create nh-log-buffer)
        (goto-char (point-max))
        (insert string))))

(defvar nh-at-prompt nil)
(defvar nh-at-prompt-hook nil
  "Called when there is a prompt. Takes one arg: the kind of prompt. Either \"command\" or \"menu\"")
(defun nh-filter (proc string)
  "Insert contents of STRING into the buffer associated with PROC.
Evaluate the buffer contents if we are looking at a prompt and then
delete the contents, perhaps logging the text."
  ;; insert output into process buffer
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)
    (forward-line 0)
    (if (looking-at nh-prompt-regexp)
        (let ((prompt (match-string 1)))
          (nh-log (buffer-substring (point-min) (point)))
          (save-restriction
            (narrow-to-region (point-min) (point))
            (eval-buffer))
          (cond ((or (equal prompt "command")
                     (equal prompt "menu")
                     (equal prompt "dummy"))
                 (nh-print-status)
                 (sit-for 0)
                 (setq nh-at-prompt t)
                 (run-hook-with-args 'nh-at-prompt-hook prompt)))))))

(defun nh-send (form)
  (let ((command (cond
                  ((null form) "()") ; the process doesn't handle `nil'
                  ((stringp form) form)
                  (t (prin1-to-string form)))))
    (with-current-buffer (process-buffer nh-proc) (erase-buffer))
    (process-send-string nh-proc (concat command "\n"))
    (nh-log (format ";;; %s\n" command))))

(defun nh-send-and-wait (form)
  (nh-send form)
  ;; wait until we get back to a "command" prompt before returning
  (setq nh-at-prompt nil)
  (while (and (member (process-status nh-proc) '(open run))
              (not nh-at-prompt))
    (accept-process-output nh-proc)))

;;; Buffer code (aka windows in Nethack)
(defvar nh-map-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w   " table)
    (modify-syntax-entry ?\) "w   " table)
    (modify-syntax-entry ?\[ "w   " table)
    (modify-syntax-entry ?\] "w   " table)
    (modify-syntax-entry ?{ "w   " table)
    (modify-syntax-entry ?} "w   " table)
    table)
  "Syntax table used in the Nethack map.")

(defun nh-map-mode ()
  "Major mode for the main Nethack map window.

\\{nh-map-mode-map}"
  (use-local-map nh-map-mode-map)
  (set-syntax-table nh-map-mode-syntax-table)
  (setq mode-name "NETHACK MAP")
  (setq major-mode 'nh-map-mode)
  ;; make scroll-other-window work on the message buffer
  (make-variable-buffer-local 'other-window-scroll-buffer)
  (setq other-window-scroll-buffer nh-message-buffer)
  (run-hooks 'nethack-map-mode-hook))

(define-derived-mode nh-message-mode text-mode "Nethack Messages"
  "Major mode for the Nethack message window"
  (setq buffer-read-only t))
(put 'nh-message-mode 'mode-class 'special)

(define-derived-mode nh-status-mode nil "Nethack Status"
  "Major mode for the Nethack status window"
  (setq buffer-read-only t))
(put 'nh-status-mode 'mode-class 'special)

(defun nethack-kill-buffers ()
  "Kill all nethack associated buffers except the nethack process
buffer."
  (when (buffer-live-p nh-map-buffer)
    (kill-buffer nh-map-buffer))        ; Preserve window for raw-print goodbye
  (dolist (buffer (list nh-status-buffer nh-message-buffer))
    (with-current-buffer buffer
      (kill-buffer-and-window)))
  (mapc (lambda (x) (when (buffer-live-p (cdr x))
                      (kill-buffer (cdr x))))
        nh-menu-buffer-table)
  (kill-buffer (get-buffer nh-log-buffer)))



(run-hooks 'nethack-load-hook)

(provide 'nethack)


;;; VERSION:
(defconst nethack-el-version "0.9.0")
(defun nethack-el-version ()
  (interactive)
  (message (format "nethack-el %s" nethack-el-version)))

;;; nethack.el ends here
