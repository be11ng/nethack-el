;;; nethack.el -- run Nethack as an inferior process in Emacs
;;; Author: Ryan Yeske (rcyeske@vcn.bc.ca)
;;; Date: Sat Mar 18 11:31:52 2000
;;; $Id$
;;; Requires: a copy of Nethack 3.3.x with the lisp window port

;;; Commentary:
;; 
(require 'nethack-api)
(require 'nethack-cmd)
(require 'nethack-keys)

;;; Code:
(defgroup nethack nil
  "Emacs lisp frontend to the lisp window port of Nethack 3.3.x."
  :group 'games)

(defgroup nethack-faces nil
  "Customizations for faces used by Enethack."
  :group 'nethack)

(defcustom nethack-load-hook nil
    "Hook run after loading nethack."
    :type '(hook)
    :group 'nethack)

;; FIXME: what should this look like?
(defface nethack-black-face
  `((t (:foreground "blue")))
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
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "lime green"))
    (t (:foreground "gray")))
  "nethack green"
  :group 'nethack-faces)

(defface nethack-brown-face
  `((((type tty) (class color))
     (:foreground "yellow"))
    (((class color))
     (:foreground "brown"))
    (t (:foreground "gray")))
  "nethack brown"
  :group 'nethack-faces)

(defface nethack-blue-face
  `((((type tty) (class color))
     (:foreground "blue"))
    (((class color))
     (:foreground "blue"))
    (t (:foreground "gray")))
  "nethack blue"
  :group 'nethack-faces)

(defface nethack-magenta-face
  `((((type tty) (class color))
     (:foreground "magenta"))
    (((class color))
     (:foreground "magenta"))
    (t (:foreground "gray")))
  "nethack magenta"
  :group 'nethack-faces)

(defface nethack-cyan-face
  `((((type tty) (class color))
     (:foreground "cyan"))
    (((class color) (background dark))
     (:foreground "cyan"))
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
     (:foreground "lightgreen"))
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
     (:foreground "lightblue"))
    (((class color) (background light))
     (:foreground "dodger blue"))
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

(defface nethack-map-glyph-face 
  `((((type tty)) 
     nil)
    (t (:font "6x10")))
  "Nethack map face for keeping glyphs from separating due to the
newlines being in a font with height > 16."
  :group 'nethack-faces)

(defvar nethack-use-glyphs nil
  "If non-nil, use XPMs to draw glyphs.")

(defun nethack-toggle-glyphs ()
  "Toggle the use of glyphs on the map."
  (interactive)
  (setq nethack-use-glyphs (not nethack-use-glyphs))
  (nethack-command-redraw-screen 2))

(defconst nethack-colors
  [nethack-black-face 		nethack-red-face
   nethack-green-face 		nethack-brown-face
   nethack-blue-face 		nethack-magenta-face
   nethack-cyan-face 		nethack-gray-face
   nethack-dark-gray-face 	nethack-orange-face
   nethack-bright-green-face 	nethack-yellow-face
   nethack-bright-blue-face 	nethack-bright-magenta-face
   nethack-bright-cyan-face 	nethack-white-face]
  "Vector indexed by Nethack's color number.")


(defvar nh-proc nil)
(defvar nh-proc-buffer-name "*nethack-output*")
(defvar nh-log-process-text t)

(defun nethack ()
  "Start a game of Nethack.

The variable `nethack-program' is the name of the executable to run."
  (interactive)
  (if (and (processp nh-proc)
	   (eq (process-status nh-proc) 'run))
      (progn
	(nethack-restore-window-configuration)
	(message "Nethack process already running..."))
    (save-excursion
      ;; Reset intermediate variables.
      (setq nethack-status-alist nil)
      ;;; Start the process.
      (if (get-buffer nh-proc-buffer-name)
	  (kill-buffer nh-proc-buffer-name))
      (setq nh-proc
	    (apply 'start-process "nh" nh-proc-buffer-name
		   nethack-program nethack-program-args))
      (set-process-filter nh-proc 'nh-filter)
      (set-process-sentinel nh-proc 'nh-sentinel)
      (make-variable-buffer-local 'nethack-buffer-type)))) ; FIXME: obsolete?


;;;; Process code to communicate with the Nethack executable
(defconst nh-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

(defcustom nethack-program "nethack"
  "Program to run to start a game of Nethack."
  :type '(string)
  :group 'nethack)

(defcustom nethack-program-args nil
  "Arguments to pass to `nethack-program'."
  :type '(repeat string)
  :group 'nethack)

(defun nh-sentinel (proc msg)
  "Nethack background process sentinel.
PROC is the process object and MSG is the exit message."
  (with-current-buffer (process-buffer proc)
    (nh-log (buffer-substring (point-min) (point)))
    (eval-region (point-min) (point-max))
    (insert "Nethack " msg)
    (pop-to-buffer (current-buffer)))
  (delete-process proc))

(defun nh-log (string)
  (if nh-log-process-text
      (with-current-buffer (get-buffer-create "*nh-log*")
	(goto-char (point-max))
	(insert string))))

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
	  (eval-region (point-min) (point))
	  (nethack-print-status)
	  (cond ((or (equal prompt "command")
		     (equal prompt "menu"))
		 (sit-for 0)
		 (setq nh-at-prompt t)))))))

(defvar nh-at-prompt nil)
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
  (while (and (eq (process-status nh-proc) 'run)
	      (not nh-at-prompt))
    (accept-process-output nh-proc)))

;;; Buffer code (aka windows in Nethack)

(defun nethack-buffer (id)
  "Return the buffer that corresponds to the Nethack window ID."
  (let ((buffer (cdr (assq id nethack-buffer-table))))
    (if (buffer-live-p buffer)
	buffer
      'nobuffer)))

;;; Main Map Buffer code
(defcustom nethack-map-mode-hook nil
  "Functions to be called after setting up the Nethack map."
  :type '(hook)
  :group 'nethack)

(defcustom nethack-menu-mode-hook nil
  "Functions to be called after setting up a Nethack menu."
  :type '(hook)
  :group 'nethack)

(defvar nethack-map-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "w   " table)
    (modify-syntax-entry ?\) "w   " table)
    (modify-syntax-entry ?\[ "w   " table)
    (modify-syntax-entry ?\] "w   " table)
    (modify-syntax-entry ?{ "w   " table)
    (modify-syntax-entry ?} "w   " table)
    table)
  "Syntax table used in the Nethack map.")


(defun nethack-map-mode ()
  "Major mode for the main Nethack map window.

\\{nethack-map-mode-map}"
  (use-local-map nethack-map-mode-map)
  (set-syntax-table nethack-map-mode-syntax-table)
  (setq mode-name "NETHACK MAP")
  (setq major-mode 'nethack-map-mode)
  (run-hooks 'nethack-map-mode-hook))

(defvar nethack-map-width 79 "Max width of the map.")
(defvar nethack-map-height 22 "Max height of the map.")


;;; status line handling

(defcustom nethack-status-highlight-delay 5
  "The number of turns to keep a changed status field highlighted."
  :type '(integer)
  :group 'nethack)

(defvar nethack-status-alist nil
  "An alist of the players status.")

(defcustom nethack-status-format
  "%n\nSt:%s Dx:%d Co:%c In:%i Wi:%w Ch:%c %a\nDlvl:%D $:%z HP:%h(%H) Pw:%p(%P) AC:%m Xp:%e(%E) T:%t %u %C %S %b %T %A %L %N"
  "The nethack status format string."
  :type '(string)
  :group 'nethack)


;;; utility/compatibility functions
(defun nethack-propertize (string &rest properties)
  "Add text PROPERTIES to STRING and return the new string."
  (add-text-properties 0 (length string) properties string)
  string)

(defun nethack-assq-delete-all (key alist)
  "Delete from ALIST all elements whose car is KEY.
Return the modified alist."
  ;; this is defined in emacs21 as `assq-delete-all'.
  (let ((tail alist))
    (while tail
      (if (eq (car (car tail)) key)
	  (setq alist (delq (car tail) alist)))
      (setq tail (cdr tail)))
    alist))

(defun nethack-window-buffer-height (window)
  "Return the height (in screen lines) of the buffer that WINDOW is displaying."
  (save-excursion
    (set-buffer (window-buffer window))
    (count-lines (point-min) (point-max))))

(defun nethack-char-to-int (char)
  ;; XEmacs chars are not ints
  (if (fboundp 'char-to-int)
      (char-to-int char)
    char))

(defun nethack-read-char (&optional prompt)
  (message prompt)
  (let ((char (read-char-exclusive)))
    (message "")
    (nethack-char-to-int char)))

(run-hooks 'nethack-load-hook)

(provide 'nethack)

;;; nethack.el ends here
