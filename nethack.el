;;; nethack.el --- run Nethack as an inferior process in Emacs
;;; Author: Ryan Yeske (rcyeske@vcn.bc.ca)
;;; Date: Sat Mar 18 11:31:52 2000

;;; Requires: a copy of Nethack 3.3.x with the lisp window port


(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)

(defvar nethack-load-hook nil
    "Run after loading nethack.
You can customize key bindings or load extensions with this.")

;; FIXME: dirty hack:
(defvar nethack-status-line-number 0
  "The line that will be updated in the status window next time
`nethack-api-putstr' is called.")

(defvar nethack-status-lines '("" . "")
  "The 2 lines of the status window")


(defface nethack-red
  `((((type tty) (class color))
     (:foreground "red"))
    (((class color))
     (:foreground "red"))
    (t (:foreground "gray")))
  "nethack red")

(defface nethack-green
  `((((type tty) (class color))
     (:foreground "green"))
    (((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "nethack green")

(defface nethack-brown
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "nethack brown")

(defface nethack-blue
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "nethack blue")

(defface nethack-magenta
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack magenta")

(defface nethack-cyan
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan4"))
    (t (:foreground "gray")))
  "nethack cyan")

(defface nethack-gray
  `((((type tty) (class color))
     (:foreground "white"))
    (((class color) (background dark))
     (:foreground "gray"))
    (((class color) (background light))
     (:foreground "gray20"))
    (t (:foreground "gray")))
  "nethack gray")

(defface nethack-dark-gray
  `((((type tty) (class color))
     (:foreground "black" :bold t))
    (((class color))
     (:foreground "gray50"))
    (t (:foreground "gray")))
  "nethack dark gray")

(defface nethack-orange
  `((((type tty) (class color))
     (:foreground "red" :bold t))
    (((class color))
     (:foreground "orange"))
    (t (:foreground "gray")))
  "nethack light orange")

(defface nethack-bright-green
  `((((type tty) (class color))
     (:foreground "green" :bold t))
    (((class color) (background dark))
     (:foreground "lightgreen"))
    (((class color) (background light))
     (:foreground "dark green"))
    (t (:foreground "gray")))
  "nethack bright green")

(defface nethack-yellow
  `((((type tty) (class color))
     (:foreground "yellow" :bold t))
    (((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "yellow3"))
    (t (:foreground "gray")))
  "nethack yellow")

(defface nethack-bright-blue
  `((((type tty) (class color))
     (:foreground "blue" :bold t))
    (((class color) (background dark))
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "dodger blue"))
    (t (:foreground "gray")))
  "nethack bright blue")

(defface nethack-bright-magenta
  `((((type tty) (class color))
     (:foreground "magenta" :bold t))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack bright magenta")

(defface nethack-bright-cyan
  `((((type tty) (class color))
     (:foreground "cyan" :bold t))
    (((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "cyan3"))
    (t (:foreground "gray")))
  "nethack bright cyan")

(defface nethack-white
  `((((type tty) (class color))
     (:foreground "white" :bold t))
    (((class color) (background dark))
     (:foreground "white"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:foreground "gray")))
  "nethack white")

(defconst nethack-color-alist 
  '((0 . nethack-black)
    (1 . nethack-red)
    (2 . nethack-green)
    (3 . nethack-brown)
    (4 . nethack-blue)
    (5 . nethack-magenta)
    (6 . nethack-cyan)
    (7 . nethack-gray)
    (8 . nethack-dark-gray)
    (9 . nethack-orange)
    (10 . nethack-bright-green)
    (11 . nethack-yellow)
    (12 . nethack-bright-blue)
    (13 . nethack-bright-magenta)
    (14 . nethack-bright-cyan)
    (15 . nethack-white))
  "An alist of nethack's color number and the corresponding face.")


(defun nethack ()
  "Start a game of Nethack.

The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (and (processp nethack-process)
	   (eq (process-status nethack-process) 'run))
      (message "Nethack process already running...")

    (make-variable-buffer-local 'nethack-buffer-type)

    ;; clean up old buffers
    (mapc (lambda (b) (kill-buffer (cdr b))) nethack-buffer-table)
    (setq nethack-buffer-table nil)
    (if (get-buffer nethack-raw-print-buffer-name)
	(kill-buffer nethack-raw-print-buffer-name))

    (setq nethack-waiting-for-command-flag nil) ;move these to nethack-mode
    (setq nethack-command-queue nil)

    (delete-other-windows)
    (setq nethack-process (nethack-start-program))))


(defun nethack-quit ()
  "Quit nethack."
  (interactive)
  (kill-process nethack-process))

;;; Process code to communicate with the Nethack executable
(defvar nethack-process nil)

(defvar nethack-program "nethack"
  "* Program to run to start a game of Nethack.")

(defvar nethack-program-args nil
  "* Arguments to pass to `nethack-program'.")

(defvar nethack-process-buffer-name "*nethack-process*"
  "Name of the buffer used to communicate with `nethack-program'.")

(defvar nethack-process-name "nethack")

(defun nethack-start-program ()
  "Start `nethack-program' with `nethack-program-args' as an
asynchronous subprocess.  Returns a process object."
  (save-excursion
    (set-buffer (get-buffer-create nethack-process-buffer-name))
    (erase-buffer))
  (let ((proc (apply 'start-process nethack-process-name
		     nethack-process-buffer-name
		     nethack-program nethack-program-args)))
    (set-process-filter proc 'nethack-process-filter)
    proc))

(defun nethack-process-send-string (string)
  "Send a STRING to the running `nethack-process'.  Appends a newline
char to the STRING."
  (let ((string-to-send (concat string "\n")))
    (if (and (processp nethack-process)
	     (eq (process-status nethack-process) 'run))
	(progn
	  ;; log the command in the process buffer
	  (nethack-log-string (concat " => " string))

	  ;; send it...
	  (process-send-string nethack-process string-to-send))
      (error "Nethack process not running"))))

(defun nethack-process-send (form)
  "Send lisp FORM to the running `nethack-process'."
  (nethack-process-send-string
   (if (not form)
       "()"				; the process can't handle '()'
     (prin1-to-string form))))

(defun nethack-process-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert string)

    (let (old-mark)
      (condition-case ()
	  (while t
	    (setq oldpos (marker-position (process-mark proc)))
	    (let* ((form (read (process-mark proc)))
		   (retval (save-excursion (eval form))))
	      (cond ((eq retval 'void))
		    ((eq retval 'unimplemented)
		     (ding)
		     (message "nethack: unimplemented: `%s'" form))
		    (t (nethack-process-send retval)))))
	(end-of-file
	 (set-marker (process-mark proc) oldpos))))))

(defun nethack-log-string (str)
  "Write STR into `nethack-process-buffer'."
  (with-current-buffer (process-buffer nethack-process)
    (goto-char (- (point-max) 1))
    (insert " ; " str)
    (set-marker (process-mark nethack-process) (point))))


;;; Buffer code (aka windows in Nethack)

(defun nethack-buffer (id)
  "Returns the buffer that corresponds to the Nethack window ID."
  (cdr (assq id nethack-buffer-table)))

;;; Main Map Buffer code
(defvar nethack-command-queue nil
  "List of strings held to be sent to the running `nethack-process' in
response to the next call to `nethack-api-get-command'.")


(defvar nethack-waiting-for-command-flag nil
  "True if the nethack process is waiting for a command.")


(defun nethack-handle-command (cmd &optional n)
  "If the nethack process is waiting for a command, send CMD to the
nethack process.  Otherwise, add CMD to `nethack-command-queue' for
eventual delivery to the running nethack process. N is the number of
times the command should be executed."
  (interactive)
  (if (null n) (setq n 1))
  (if nethack-waiting-for-command-flag
      (progn
	(nethack-process-send-string (concat cmd " " (int-to-string n)))
	(setq nethack-waiting-for-command-flag nil))
    (setq nethack-command-queue
	  (nconc nethack-command-queue
		 (list (concat cmd " " (int-to-string n)))))))


(defvar nethack-map-mode-hook nil
  "Functions to run after setting up the nethack-map mode.")


(defun nethack-map-mode ()
  "Major mode for the main Nethack map window.

\\{nethack-mode-map}"
  (use-local-map nethack-mode-map)
  (setq mode-name "NETHACK MAP")
  (setq major-mode 'nethack-map-mode)

  ;; turn off show-paren-mode in this buffer
  (make-local-variable 'show-paren-mode)
  (show-paren-mode -1)

  (run-hooks 'nethack-map-mode-hook))
 

(defvar nethack-map-width 79 "Max width of the map")
(defvar nethack-map-height 22 "Max height of the map")

;;; Functions to restore nethack window configurations

;; (defun nethack-restore-windows ()
;;   "Restore a standard nethack window configuration."
;;   (interactive)
;;   (let ((new-win))
;;     (delete-other-windows)
;;     (set-window-buffer (selected-window) 
;; 		       (cdr (assoc 'nhw-map
;; 				   nethack-buffer-name-alist)))
;;     (setq new-win (split-window nil 4))
;;     (set-window-buffer (selected-window) 
;; 		       (cdr (assoc 'nhw-message
;; 				   nethack-buffer-name-alist)))
;;     (select-window new-win)
;;     (let ((window-min-height 3))
;;       (setq new-win (split-window nil (- (window-height) 3))))
;;     (set-window-buffer new-win
;; 		       (cdr (assoc 'nhw-status
;; 				   nethack-buffer-name-alist)))))

(provide 'nethack)

(run-hooks 'nethack-load-hook)		; for your customizations

;;; nethack.el ends here
