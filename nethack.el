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
;;(require 'nethack-glyphs)

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
      (if (get-buffer "*nethack-output*")
	  (kill-buffer "*nethack-output*"))
      ;; pop to buffer so if there is an error right away the user can
      ;; see what the output from the process was
      ;;(pop-to-buffer "*nh*")
      (setq nh-proc
	    (apply 'start-process "nh" nil nethack-program nethack-program-args))
;;      (nh-comint-mode)
;;      (setq nh-comint-proc (get-buffer-process (current-buffer)))
      (set-process-filter nh-proc 'nh-filter)
      (set-process-sentinel nh-proc 'nh-sentinel)
      (make-variable-buffer-local 'nethack-buffer-type)))) ; FIXME: obsolete?


;;;; Process code to communicate with the Nethack executable
(defvar nh-prompt-regexp
  "^\\(command\\|menu\\|dummy\\|direction\\|number\\|string\\)> *")

; (define-derived-mode nh-comint-mode comint-mode "Nethack Process"
;   (make-local-variable 'comint-prompt-regexp)
;   (setq comint-prompt-regexp nh-prompt-regexp))

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
  (with-current-buffer (get-buffer-create "*nethack-output*")
    (eval-region (point-min) (point-max))
    (insert "Nethack " msg)
    (pop-to-buffer (current-buffer)))
  (delete-process proc))

(defun nh-filter (proc string)
  "Insert contents of STRING into the buffer associated with PROC.
Evaluate the buffer contents between the last prompt and the current
position if we are looking at a prompt."
  ;; insert output into process buffer
  (with-current-buffer (get-buffer-create "*nethack-output*")
    (goto-char (point-max))
    (insert string)
    (forward-line 0)
    (if (looking-at nh-prompt-regexp)
	(let ((prompt (match-string 1)))
	  (eval-region (point-min) (point))
	  (erase-buffer)
	  (cond ((or (equal prompt "command")
		     (equal prompt "menu"))
		 (sit-for 0)
		 (setq nh-at-prompt t)))))))

(defvar nh-at-prompt nil)
(defun nh-send (form)
;;  (if (buffer-name (process-buffer nh-proc))
;;      (save-excursion
;;	(set-buffer (process-buffer nh-proc))
;;	(save-restriction
;;	  (widen)
;;	  (goto-char (point-max))
	  (process-send-string 
	   nh-proc 
	   (concat (cond 
		    ((null form) "()")	; the process doesn't handle `nil'
		    ((stringp form) form)
		    (t (prin1-to-string form)))
		   "\n")))
;;	  (comint-send-input))
;;    (error "No nethack process")))
  
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

  ;; make sure show-paren-mode is off in this buffer
  ;; FIXME: do this with syntax tables or something
;;  (make-local-variable 'show-paren-mode)
;;  (show-paren-mode -1)

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

(defun nethack-parse-status-lines (line-1 line-2)
  (let ((regexp-line-1 "\\(\\(?:\\w+\\s-\\)*\\w+\\)+\\s-+St:\\([0-9]+\\(?:\\/[0-9]+\\)?\\)\\s-+Dx:\\([0-9]+\\)\\s-+Co:\\([0-9]+\\)\\s-+In:\\([0-9]+\\)\\s-+Wi:\\([0-9]+\\)\\s-+Ch:\\([0-9]+\\)\\s-+\\(\\w+\\)")
	(regexp-line-2 "Dlvl:\\([0-9]+\\)\\s-+\\$:\\([0-9]+\\)\\s-+HP:\\([0-9]+\\)\\s(\\([0-9]+\\)\\s)\\s-+Pw:\\([0-9]+\\)\\s(\\([0-9]+\\)\\s)\\s-+AC:\\(-?[0-9]+\\)\\s-+Xp:\\([0-9]+\\)\\/\\([0-9]+\\)\\s-+T:\\([0-9]+\\)")
	(symbols-1 '(name strength dexterity constution intelligence wisdom charisma alignment))
	(symbols-2 '(dungeon-level zorkmids hitpoints max-hitpoints power max-power armor-class experience experience-level-up time))
	(status)
	(count))

    ;; Parse the first line
    (string-match regexp-line-1 line-1)
    (setq count (length symbols-1))
    (while (> count 0)
      (let* ((old-status (assoc (elt symbols-1 (1- count)) nethack-status-alist))
	     (old-highlight-delay (if (elt old-status 2)
				      (elt old-status 2)
				    0))
	     (data-changed (not (string-equal (match-string count line-1)
					      (elt old-status 1)))))
      (push (list (elt symbols-1 (1- count))
		  (if (match-string count line-1)
		      (match-string count line-1)
		    "")
		  (if data-changed
		      (* (if (eq (elt symbols-2 (1- count)) 'armor-class)
			     -1 1)
			 (if (< (string-to-int (match-string count line-1))
				(if (null (elt old-status 1))
				    0
				  (string-to-int (elt old-status 1))))
			     (- nethack-status-highlight-delay)
			   nethack-status-highlight-delay))
		    (cond ((> old-highlight-delay 0)
			   (1- old-highlight-delay))
			  ((< old-highlight-delay 0)
			   (1+ old-highlight-delay))
			  (t 0))))
	    status)
      (setq count (1- count))))

    ;; Parse the second line
    (string-match regexp-line-2 line-2)
    (setq count (length symbols-2))
    (while (> count 0)
      (let* ((old-status (assoc (elt symbols-2 (1- count)) nethack-status-alist))
	     (old-highlight-delay (if (elt old-status 2)
				      (elt old-status 2)
				    0))
	     (data-changed (not (string-equal (match-string count line-2)
					      (elt old-status 1)))))
	(push (list (elt symbols-2 (1- count))
		    (if (match-string count line-2)
			(match-string count line-2)
		      "")
		    (if data-changed
			(* (if (eq (elt symbols-2 (1- count)) 'armor-class)
			       -1 1)
			   (if (< (string-to-int (match-string count line-2))
				  (if (null (elt old-status 1))
				      0
				    (string-to-int (elt old-status 1))))
			       (- nethack-status-highlight-delay)
			     nethack-status-highlight-delay))
		      (cond ((> old-highlight-delay 0)
			     (1- old-highlight-delay))
			    ((< old-highlight-delay 0)
			     (1+ old-highlight-delay))
			    (t 0))))
	      status)
	(setq count (1- count))))

    ;; Fill in the flags
    (mapcar (function (lambda (pair)
			(let* ((old-status (assoc (car pair) nethack-status-alist))
			       (old-highlight-delay (if (elt old-status 2)
							(elt old-status 2)
						      0)))
			  (if (string-match (cdr pair) line-2)
			      (add-to-list 'status (list (car pair)
							 (match-string 0 line-2)
							 (if (not (string-equal (match-string 0 line-2)
										(elt old-status 1)))
							     (- nethack-status-highlight-delay)
							   (if (< old-highlight-delay 0)
							       (1+ old-highlight-delay)
							     0))))
			    (add-to-list 'status (list (car pair) "" 0))))))
	    '((hungry . "Satiated\\|Hungry\\|Weak\\|Fainting\\|Fainted\\|Starved")
	      (confused . "Conf")
	      (sick . "Sick")
	      (blind . "Blind")
	      (stunned . "Stun")
	      (hallucinating . "Hallu")
	      (slimed . "Slime")
	      (encumbrance . "Burdened\\|Stressed\\|Strained\\|Overtaxed\\|Overloaded")))

    (setq nethack-status-alist status)))

(defun nethack-format-status (fmt)
  "Return a string containing the player status.  FMT is the format string."
    (let ((str fmt)
	  (match-phrase '((name . "%n")
			  (strength . "%s")
			  (dexterity . "%d")
			  (constution . "%c")
			  (intelligence . "%i")
			  (wisdom . "%w")
			  (charisma . "%c")
			  (alignment . "%a")
			  (hungry . "%u")
			  (confused . "%C")
			  (sick . "%S")
			  (blind . "%b")
			  (stunned . "%T")
			  (hallucinating . "%A")
			  (slimed . "%L")
			  (encumbrance . "%N")
			  (dungeon-level . "%D")
			  (zorkmids . "%z")
			  (hitpoints . "%h")
			  (max-hitpoints . "%H")
			  (power . "%p")
			  (max-power . "%P")
			  (armor-class . "%m")
			  (experience . "%e")
			  (experience-level-up . "%E")
			  (time . "%t"))))
      (mapcar (function (lambda (l)
			  (let ((case-fold-search nil)
				(start 0))
			    (when (string-match (cdr (assoc (elt l 0) match-phrase)) str)
			      (setq str (replace-match (cond ((> (elt l 2) 0)
							      (propertize (elt l 1)
									  'face 'nethack-green-face))
							     ((< (elt l 2) 0)
							      (propertize (elt l 1)
									  'face 'nethack-red-face))
							     (t (elt l 1)))
						       t t str))))))
	      nethack-status-alist)
      str))


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


(run-hooks 'nethack-load-hook)

(provide 'nethack)

;;; nethack.el ends here
