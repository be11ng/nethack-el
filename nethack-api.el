;;; nethack-api.el -- low level Emacs interface the lisp window-port
;;; of Nethack-3.3.x
;;; $Id: nethack-api.el,v 1.59 2002/01/14 10:32:14 rcyeske Exp $

;;; originally a machine translation of nethack-3.3.0/doc/window.doc
;;; from the nethack src package.

;;; Ryan Yeske (rcyeske@vcn.bc.ca)
;;; Sat Mar 18 11:24:02 2000


;;; Commentary:
;; 

(require 'ewoc)
(require 'gamegrid)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Introduction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file documents the support for various windowing systems in
;; NetHack.  The support is through a standard interface, separating
;; the main NetHack code from window-system specific code.  The
;; implementation supports multiple window systems in the same binary.
;; Even if you only wish to support one window-port on your port, you
;; will need to follow the instructions in Section VII to get a
;; compilable binary.

;; Contents:
;; 	I.   Window Types and Terminology
;; 	II.  Interface Specification
;; 	III. Global variables
;; 	IV.  New or respecified common, high level routines
;; 	V.   Game startup
;; 	VI.  Conventions
;; 	VII. Implementation and Multi-window support

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I.  Window Types and Terminology
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are 5 basic window types, used to call create_nhwindow():

;;; FIXME: what are the values for these
;; (defconst nethack-api-wmessage nil)	;NHW_MESSAGE (top line)
;; (defconst nethack-api-wstatus nil)	;NHW_STATUS	(bottom lines)
;; (defconst nethack-api-wmap nil)	;NHW_MAP(main dungeon)
;; (defconst nethack-api-wmenu nil)	;NHW_MENU (inventory or other
;; 					;"corner" windows)
;; (defconst nethack-api-wtext nil)	;NHW_TEXT (help/text, full
					;screen paged window)

;; The tty window-port also uses NHW_BASE (the base display)
;; internally.

;; NHW_MENU windows can be used for either menu or text display.
;; Their basic feature is that for the tty-port, if the window is
;; small enough, it appears in the corner of the tty display instead
;; of overwriting the whole screen.  The first call to add information
;; to the window will decide if it is going to be used to display a
;; menu or text.  If start_menu() is called, then it will be used as a
;; menu.  If putstr() is called, it will be used as text.  Once
;; decided, there is no turning back.  For the tty-port, if the data
;; is too large for a single screen then the data is paged (with
;; --more--) between pages.  Only NHW_MENU type windows can be used
;; for menus.

;; NHW_TEXT windows are used to display a large amount of textual
;; data.  This is the type of window one would use for displaying a
;; help file, for example.  In the tty window-port, windows of type
;; NHW_TEXT can page using the DEF_PAGER, if DEF_PAGER is defined.
;; There exists an assumption that the font for text windows is
;; monospaced.  The help files are all formatted accordingly.

;; "window" is always of type winid.  This is currently implemented as
;; an integer, but doesn't necessarily have to be done that way.
;; There are a few fixed window names that are known throughout the
;; code:

;;; FIXME: what are the values for these
;; (defconst nethack-api-win-message nil);	WIN_MESSAGE (top line)
;; (defconst nethack-api-win-status nil) ;	WIN_STATUS (bottom lines)
;; (defconst nethack-api-win-map nil)	;	WIN_MAP (main dungeon)
;; (defconst nethack-api-win-inven nil)	;	WIN_INVEN (inventory)

;; Other windows are created and destroyed as needed.

;; "Port" in this document refers to a CPU/OS/hardware platform (UNIX, MSDOS
;; TOS, etc.)  "window-port" refers to the windowing platform.  This is
;; orthogonal (e.g.  UNIX might use either a tty window-port or an X11
;; window-port).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; II.  Interface Specification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All functions below are void unless otherwise noted.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A.  Low-level routines:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(defvar nethack-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

;; raw_print(str) -- Print directly to a screen, or otherwise
;; guarantee that the user sees str.  raw_print() appends a newline to
;; str.  It need not recognize ASCII control characters.  This is used
;; during startup (before windowing system initialization -- maybe this
;; means only error startup messages are raw), for error messages, and
;; maybe other 'msg' uses.  E.g.  updating status for micros (i.e,
;; 'saving').

(defun nethack-api-raw-print (str)
  (save-current-buffer
    (let ((buffer (get-buffer-create nethack-raw-print-buffer-name)))
      (pop-to-buffer nethack-message-buffer)
      (delete-other-windows)
      (insert str "\n"))))

;; raw_print_bold(str) -- Like raw_print(), but prints in
;; bold/standout (if possible).

(defun nethack-api-raw-print-bold (str)
  (nethack-api-raw-print
   (propertize str 'face (nethack-attr-face 'atr-bold))))


;; curs(window, x, y) -- Next output to window will start at (x,y),
;; also moves displayable cursor to (x,y).  For backward compatibility, 1
;; <= x < cols, 0 <= y < rows, where cols and rows are the size of
;; window.  -- For variable sized windows, like the status window, the
;; behavior when curs() is called outside the window's limits is
;; unspecified. The mac port wraps to 0, with the status window being 2
;; lines high and 80 columns wide.  -- Still used by curs_on_u(), status
;; updates, screen locating (identify, teleport).  -- NHW_MESSAGE,
;; NHW_MENU and NHW_TEXT windows do not currently support curs in the tty
;; window-port.

(defun nethack-api-curs (winid x y)
  ""
  (with-current-buffer (nethack-buffer winid)
    (cond ((eq nethack-buffer-type 'nhw-map)
	   (goto-char (gamegrid-cell-offset (- x 1) y)))
	  ((eq nethack-buffer-type 'nhw-status)
	   (setq nethack-status-line-number y)))))

;; is this the way to make a "default" face?
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

(defcustom nethack-status-in-modeline nil
  "If non-nil, display the status in the modeline of the buffer
containing the map.

Since the modeline can only display 1 line, you must make sure there
are no newlines in `nethack-status-string'."
  :type '(boolean)
  :group 'nethack)

(defun nethack-print-status-string (alist attr fmt)
  (let ((new-value (cadr (assoc attr alist))))
    (if new-value
	(progn
	  (if (numberp new-value)
	      (setq new-value (number-to-string new-value)))
	  (insert (format fmt new-value))))))

;; FIXME: this is a temporary kludge which will disappear when we have
;; real cooked status information coming from the process.
(defun nethack-api-update-status (status)
  (with-current-buffer nethack-status-buffer
    (erase-buffer)
    (insert (format "%s the " (cadr (assoc "name" status))))
    ;; Recreate nethack-status-alist and simultaneously print each attribute
    (setq nethack-status-alist
	  (list
	   (if (assoc "rank" status)
	       (nethack-print-status-string status "rank" "%s  ")
	     (nethack-print-status-string status "monster" "%s  "))
	   (nethack-print-status-string status "St" "St:%s ")
	   (nethack-print-status-string status "Dx" "Dx:%s ")
	   (nethack-print-status-string status "Co" "Co:%s ")
	   (nethack-print-status-string status "In" "In:%s ")
	   (nethack-print-status-string status "Wi" "Wi:%s ")
	   (nethack-print-status-string status "Ch" "Ch:%s ")
	   (nethack-print-status-string status "Align" " %s\n")
	   (nethack-print-status-string status "Dungeon" "%s ")
	   (nethack-print-status-string status "Dlvl" "Dlvl:%s ")
	   (nethack-print-status-string status "$" "$:%s ")
	   (nethack-print-status-string status "HP" "HP:%s")
	   (nethack-print-status-string status "HPmax" "(%s) ")
	   (nethack-print-status-string status "PW" "Pw:%s")
	   (nethack-print-status-string status "PWmax" "(%s) ")
	   (nethack-print-status-string status "AC" "AC:%s ")
	   (if (assoc "HD" status)
	       (nethack-print-status-string status "HD" "HD:%s ")
	     (nethack-print-status-string status "Level" "Xp:%s")
	     (nethack-print-status-string status "XP" "/%s "))
	   (nethack-print-status-string status "T" "T:%s ")

	   ;; Process flags. If a flag was not there before, it should be
	   ;; marked in red as all flag updates are for bad things.
	   ;; FIXME: Not quite functioning but it doesn't need CL anymore.
	   (list "Flags"
		 (mapcar (lambda (flag)
			   (let ((old-flag (assoc flag (cadr (assoc "Flags" nethack-status-alist)))))
			     (cond ((null old-flag)
				    ;; new flag, so highlight it and set the timer
				    (insert (concat (propertize flag 'face 'nethack-red-face) " "))
				    (list flag nethack-status-highlight-delay))
				   ((> (cadr old-flag) 0)
				    ;; existing flag. Highlight the flag if it hasn't timed-out yet.
				    (insert (concat (propertize flag 'face 'nethack-red-face) " "))
				    (list flag (1- (cadr old-flag))))
				   (t 
				    ;; The flag is old and its highlight has timed out. 
				    ;; Just print the flag.
				    (insert (concat flag " "))
				    old-flag))))
			 (cadr (assoc "Flags" status))))))
    ;; Add a trailing newline to the status buffer
    (insert "\n")))

;; putstr(window, attr, str) -- Print str on the window with the
;; given attribute.  Only printable ASCII characters (040-0126) must be
;; supported.  Multiple putstr()s are output on separate lines.
;; Attributes can be one of ATR_NONE (or 0) ATR_ULINE ATR_BOLD ATR_BLINK
;; ATR_INVERSE If a window-port does not support all of these, it may map
;; unsupported attributes to a supported one (e.g. map them all to
;; ATR_INVERSE).  putstr() may compress spaces out of str, break str, or
;; truncate str, if necessary for the display.  Where putstr() breaks a
;; line, it has to clear to end-of-line.  -- putstr should be implemented
;; such that if two putstr()s are done consecutively the user will see
;; the first and then the second.  In the tty port, pline() achieves this
;; by calling more() or displaying both on the same line.

(defun nethack-api-putstr (winid attr str)
  "Print STR on WINID with ATTR."
  (with-current-buffer (nethack-buffer winid)
    (let ((inhibit-read-only t))
      (cond ((eq nethack-buffer-type 'nhw-message)
	     (goto-char (point-max))
	     (insert str "\n")
	     ;; cover new text with highlight overlay
	     (let ((start (overlay-start nethack-message-highlight-overlay)))
	       (move-overlay nethack-message-highlight-overlay
			     start (point-max)))
	     ;; scroll to show maximum output on all windows displaying buffer
	     (let ((l (get-buffer-window-list (nethack-buffer winid))))
	       (save-selected-window
		 (mapc (lambda (w)
			 (select-window w)
			 (set-window-point w (- (point-max) 1))
			 (recenter -1))
		       l))))
	    (t
	     (goto-char (point-max))
	     (insert (propertize str 'face (nethack-attr-face attr))
		     "\n"))))))

(defun nethack-attr-face (attr)
  "Return the face corresponding with ATTR."
  (intern-soft (concat "nethack-" (symbol-name attr) "-face")))

;; get_nh_event() -- Does window event processing (e.g. exposure
;; events).  A noop for the tty and X window-ports.

(defun nethack-api-get-event ()
  ""
  )


;; int nhgetch() -- Returns a single character input from the
;; user. (int) -- In the tty window-port, nhgetch() assumes that tgetch()
;; will be the routine the OS provides to read a character.  Returned
;; character _must_ be non-zero.

(defun nethack-api-get-command ()
  ""
;;   (nethack-parse-status-lines (car nethack-status-lines)
;; 			      (cdr nethack-status-lines))
;;   (let ((status-string (nethack-format-status nethack-status-format)))
;;     (with-current-buffer nethack-status-buffer
;;       (erase-buffer)
;;       (insert status-string))))
)

;; int nh_poskey(int *x, int *y, int *mod) -- Returns a single
;; character input from the user or a a positioning event (perhaps from a
;; mouse).  If the return value is non-zero, a character was typed, else,
;; a position in the MAP window is returned in x, y and mod.  mod may be
;; one of CLICK_1 /* mouse click type 1 */ CLICK_2 /* mouse click type 2
;; */ The different click types can map to whatever the hardware
;; supports.  If no mouse is supported, this routine always returns a
;; non-zero character.

(defun nethack-api-poskey (x y mod)
  ""
  'unimplemented)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; B.  High-level routines:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print_glyph(window, x, y, glyph) -- Print the glyph at (x,y) on the
;; given window.  Glyphs are integers at the interface, mapped to
;; whatever the window- port wants (symbol, font, color, attributes,
;; ...there's a 1-1 map between glyphs and distinct things on the map).

(defun nethack-api-print-glyph (winid x y type offset color glyph tile ch)
  ""
;;  (with-current-buffer (nethack-buffer winid)
  (set-buffer (nethack-buffer winid))
  (setq x (- x 1)) 				; FIXME: put this hack in C
  (let ((inhibit-read-only t))
    (if nethack-use-glyphs
	(save-excursion 
	  (let ((buffer-read-only nil))
	    (goto-char (gamegrid-cell-offset x y))
	    (delete-char 1)
	    (insert-image (elt nethack-glyph-vector tile))))
      (gamegrid-set-cell x y ch)
      (put-text-property (gamegrid-cell-offset x y)
			 (1+ (gamegrid-cell-offset x y))
			 'face
			 (aref nethack-colors color)))))

;; char yn_function(const char *ques, const char *choices, char
;; default) -- Print a prompt made up of ques, choices and default.  Read
;; a single character response that is contained in choices or default.
;; If choices is NULL, all possible inputs are accepted and returned.
;; This overrides everything else.  The choices are expected to be in
;; lower case.  Entering ESC always maps to 'q', or 'n', in that order,
;; if present in choices, otherwise it maps to default.  Entering any
;; other quit character (SPACE, RETURN, NEWLINE) maps to default.  -- If
;; the choices string contains ESC, then anything after it is an
;; acceptable response, but the ESC and whatever follows is not included
;; in the prompt.  -- If the choices string contains a '#' then accept a
;; count.  Place this value in the global *yn_number* and return '#'.  --
;; This uses the top line in the tty window-port, other ports might use a
;; popup.

(defun nethack-api-yn-function (ques choices default)
  ""
  (let ((cursor-in-echo-area t)
	all-choices
	key)

    (if (= default 0)
	(setq all-choices (string-to-list choices))
      (setq all-choices (string-to-list (concat (char-to-string default)
						choices))))
    ;; Add some special keys of our own to the choices
    (push 13 all-choices)

    (message (concat ques " "))
    (setq key (read-char))

    (if (> (length choices) 0)
	(while (not (member key all-choices))
	  (message (concat ques " "))
	  (setq key (read-char))))

    (nh-send (if (= 13 key)
		 default
	       key))))

(defun nethack-api-ask-direction (prompt)
  "Prompt the user for a direction"
  (let* ((cursor-in-echo-area t)
	 (cmd (lookup-key nethack-map-mode-map
			  (vector (read-char-exclusive
				   (concat prompt " "))))))
    (nh-send
     (cond ((eq cmd 'nethack-command-north) "n")
	   ((eq cmd 'nethack-command-south) "s")
	   ((eq cmd 'nethack-command-west) "w")
	   ((eq cmd 'nethack-command-east) "e")
	   ((eq cmd 'nethack-command-northwest) "nw")
	   ((eq cmd 'nethack-command-northeast) "ne")
	   ((eq cmd 'nethack-command-southwest) "se")
	   ((eq cmd 'nethack-command-southeast) "sw")
	   ((eq cmd 'nethack-command-up) "up")
	   ((eq cmd 'nethack-command-down) "down")
	   ((eq cmd 'nethack-command-rest-one-move) "self")
	   ((eq cmd 'nethack-command-search) "self")
	   (t "nowhere")))))

;; getlin const char *ques, char *input) -- Prints ques as a prompt
;; and reads a single line of text, up to a newline.  The string entered
;; is returned without the newline.  ESC is used to cancel, in which case
;; the string '\033\000' is returned.  -- getlin() must call
;; flush_screen(1) before doing anything.  -- This uses the top line in
;; the tty window-port, other ports might use a popup.

(defun nethack-api-getlin (ques)
  ""
  (nh-send (read-from-minibuffer (concat ques " "))))


;; int get_ext_cmd(void) -- Get an extended command in a window-port
;; specific way.  An index into extcmdlist[] is returned on a successful
;; selection, -1 otherwise.

;; player_selection() -- Do a window-port specific player type
;; selection.  If player_selection() offers a Quit option, it is its
;; responsibility to clean up and terminate the process.  You need to
;; fill in pl_character[0].

(defun nethack-api-player-selection ()
  "Does nothing right now, perhaps simply indicates that the
nethack-api-choose-X calls are to follow for actual
role/race/gender/align selection.")

(defun nethack-choose-attribute (prompt alist abort)
  "Prompts user for an element from the cars of ALIST and returns the
corresponding cdr."
  (nh-send
   (if (> (length alist) 1)
       (let ((completion-ignore-case t))
	 (condition-case nil
	     (cdr (assoc (completing-read prompt alist nil t) alist))
	   (quit abort)))
     (cdar alist))))

(defun nethack-api-choose-role (role-alist)
  (nethack-choose-attribute "Choose role: " role-alist -1))

(defun nethack-api-choose-race (race-alist)
  (nethack-choose-attribute "Choose race: " race-alist -1))

(defun nethack-api-choose-gender (gender-alist)
  (nethack-choose-attribute "Choose gender: " gender-alist -1))

(defun nethack-api-choose-alignment (alignment-alist)
  (nethack-choose-attribute "Choose alignment: " alignment-alist -1))

;;  display_file(str, boolean complain) -- Display the file named str.
;; Complain about missing files iff complain is TRUE.

(defun nethack-api-display-file (str complain)
  (let ((file (concat nethack-directory str)))
    (if (file-exists-p file)
	(view-file file)
      (if complain (message "Cannot find file %s" file)))))

;; update_inventory() -- Indicate to the window port that the
;; inventory has been changed.  -- Merely calls display_inventory() for
;; window-ports that leave the window up, otherwise empty.

(defun nethack-api-update-inventory ()
  " FIXME: should set a flag or something, later"
  )

;; doprev_message() -- Display previous messages.  Used by the ^P
;; command.  -- On the tty-port this scrolls WIN_MESSAGE back one line.

(defun nethack-api-doprev-message ()
  ""
  (save-selected-window
    (save-current-buffer		; is this redundant since we
					; already save the selected
					; window? -rcy
      (walk-windows (lambda (w)
		      (select-window w)
		      (set-buffer (window-buffer))
		      (if (eq nethack-buffer-type 'nhw-message)
			  (scroll-down)))))))

;; update_positionbar(char *features) -- Optional, POSITIONBAR must be
;; defined. Provide some additional information for use in a horizontal
;; position bar (most useful on clipped displays).  Features is a series
;; of char pairs.  The first char in the pair is a symbol and the second
;; char is the column where it is currently located.  A '<' is used to
;; mark an upstairs, a '>' for a downstairs, and an '@' for the current
;; player location. A zero char marks the end of the list.

(defun nethack-api-update-positionbar (features)
  ""
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C.  Window Utility Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init_nhwindows(int* argcp, char** argv) -- Initialize the windows
;; used by NetHack.  This can also create the standard windows listed at
;; the top, but does not display them.  -- Any commandline arguments
;; relevant to the windowport should be interpreted, and *argcp and *argv
;; should be changed to remove those arguments.  -- When the message
;; window is created, the variable iflags.window_inited needs to be set
;; to TRUE.  Otherwise all plines() will be done via raw_print().  ** Why
;; not have init_nhwindows() create all of the 'standard' ** windows?  Or
;; at least all but WIN_INFO? -dean

(defvar nethack-directory nil
  "Location of the nethack directory.

This is set when the process starts by `nethack-api-init-nhwindows'.
Do not edit the value of this variable.  Instead, change the value of
`nethack-program'.")

(defun nethack-api-init-nhwindows (executable &rest args)
  "This is the first function sent by the nethack process.  Does
all of the appropriate setup."
  (setq nethack-directory (file-name-directory executable))
  ;; clean up old buffers
  (mapc (lambda (b) (kill-buffer (cdr b))) nethack-buffer-table)
  (setq nethack-buffer-table nil)
  (if (get-buffer nethack-raw-print-buffer-name)
      (kill-buffer nethack-raw-print-buffer-name)))

;; exit_nhwindows(str) -- Exits the window system.  This should
;; dismiss all windows, except the 'window' used for raw_print().  str is
;; printed if possible.

(defun nethack-api-exit-nhwindows (str)
  ""
  ;; print the message in STR to the raw print buffer
  (nethack-api-raw-print str))

;; window = create_nhwindow(type) -- Create a window of type 'type'.

(defvar nethack-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs")

;; obsolete?
(defvar nethack-buffer-type nil
  "Buffer local variable indicating the type of buffer this is.")

(defvar nethack-map-buffer nil)
(defvar nethack-status-buffer nil)
(defvar nethack-message-buffer nil)

;; The nethack-api-create-*-window functions call
;; nethack-api-create-nh-window to do the creation and common setup

(defvar nethack-message-highlight-overlay nil
  "Overlay used to highlight new text in the message window.")

(defface nethack-message-highlight-face
  '((t (:inherit highlight)))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defun nethack-api-create-message-window (id)
  "Create the message window."
  (with-current-buffer (nethack-api-create-nhwindow 'nhw-message id)
    (setq nethack-message-buffer (current-buffer))
    (setq nethack-message-highlight-overlay
	  (make-overlay (point-max) (point-max)))
    (overlay-put nethack-message-highlight-overlay 
		 'face 'nethack-message-highlight-face)))

(defun nethack-api-create-status-window (id)
  "Create the status window."
  (with-current-buffer (nethack-api-create-nhwindow 'nhw-status id)
    (setq nethack-status-buffer (current-buffer))))

(defun nethack-api-create-map-window (id)
  "Create the map window."
  (with-current-buffer (nethack-api-create-nhwindow 'nhw-map id)
    (setq nethack-map-buffer (current-buffer))
    ;; set up the gamegrid and the keymap
    (nethack-map-mode)
    ;; arrange the initial window configuration
    (nethack-restore-window-configuration)))

(defun nethack-api-create-inventory-window (id)
  "Create the inventory window."
  (nethack-api-create-menu-window id))

(defun nethack-api-create-menu-window (id)
  "Create a menu window."
  (with-current-buffer (nethack-api-create-nhwindow 'nhw-menu id)
    (setq buffer-read-only t)))

(defun nethack-api-create-text-window (id)
  "Create a text window."
  (nethack-api-create-nhwindow 'nhw-text id))

(defun nethack-api-create-nhwindow (type winid)
  "Create a new window of TYPE and WINID and add it to the buffer table.

Return the buffer."
  (let* ((name (format "*%s* %d" (symbol-name type) winid))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables)
      (setq nethack-buffer-type type))
    (push (cons winid buf) nethack-buffer-table)
    buf))


;; clear_nhwindow(window) -- Clear the given window, when
;; appropriate.

(defun nethack-api-clear-nhwindow (winid)
  ""
  (with-current-buffer (nethack-buffer winid)
    (cond ((eq nethack-buffer-type 'nhw-message)
	   ;; move overlay off the last messages
	   (move-overlay nethack-message-highlight-overlay
			 (point-max) (point-max)))
	  ((eq nethack-buffer-type 'nhw-map)
	   (let ((inhibit-read-only t))
	     (erase-buffer)
	     (if nethack-use-glyphs
		 ;; initialize the map with empty glyphs
		 (dotimes (i nethack-map-height)
		   (dotimes (j nethack-map-width)
		     (insert-image nethack-empty-glyph))
		   (insert (propertize "\n" 'face 'nethack-map-glyph-face)))
	       (gamegrid-init (make-vector 256 nil))
	       (gamegrid-init-buffer nethack-map-width
				     nethack-map-height
				     ? ))))
	  (t
	   (let ((inhibit-read-only t))
	     (erase-buffer))))))

;; display_nhwindow(window, boolean blocking) -- Display the window on
;; the screen.  If there is data pending for output in that window, it
;; should be sent.  If blocking is TRUE, display_nhwindow() will not
;; return until the data has been displayed on the screen, and
;; acknowledged by the user where appropriate.  -- All calls are blocking
;; in the tty window-port.  -- Calling display_nhwindow(WIN_MESSAGE,???)
;; will do a --more--, if necessary, in the tty window-port.

(defun nethack-api-display-nhwindow (winid blocking)
  (with-current-buffer (nethack-buffer winid)
    (if blocking
	(cond
	 ;; Both menu's and text can be filled with arbitrary message
	 ;; text.  Try to show the message in the message window,
	 ;; otherwise setup a "choiceless" menu.
	 ((or (eq nethack-buffer-type 'nhw-menu)
	      (eq nethack-buffer-type 'nhw-text))
	  (let ((message-window (get-buffer-window nethack-message-buffer))
		(message-size (count-lines (point-min) (point-max)))
		(message-winid (car (rassoc nethack-message-buffer
					    nethack-buffer-table))))
	    (if (or (not message-window)
		    (>= message-size (window-height message-window)))
		(nethack-api-select-menu winid 'pick-none)
	      (nethack-api-putstr message-winid 'atr-none
				  (buffer-substring (point-min)
						    (- (point-max) 1)))
	      (nh-send 'dummy))))
	 ;; This is called on the map when there is something that
	 ;; needs to be temporarily unhidden, for example all of the
	 ;; monsters.
	 ((eq nethack-buffer-type 'nhw-map)
	  (read-char-exclusive)
	  (nh-send 'dummy))
	 ;; At the end of the game, the message buffer is shown one
	 ;; last time, not sure why.  We should do something better
	 ;; here, but the problem is we have no way of indicating to
	 ;; the user that we are waiting for a key to move on.
	 ((eq nethack-buffer-type 'nhw-message)
	  (nh-send 'dummy)))
      ;; Do nothing for nonblocking, except use if we are displaying
      ;; the status window (which is only done once).  In that case
      ;; set up all of the windows for display for the first time.
      (if (eq nethack-buffer-type 'nhw-status)
	  (nethack-restore-window-configuration)))))


(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defun nethack-restore-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  (delete-other-windows)
  (switch-to-buffer nethack-message-buffer)
  (split-window-vertically (- nethack-message-window-height))
  (switch-to-buffer nethack-status-buffer)
  (split-window-vertically nethack-status-window-height)
  (switch-to-buffer-other-window nethack-map-buffer))

;; destroy_nhwindow(window) -- Destroy will dismiss the window if the
;; window has not already been dismissed.

(defun nethack-api-destroy-nhwindow (winid)
  (save-current-buffer
    (let ((buffer (nethack-buffer winid)))
      (delete-windows-on buffer nil)
      (kill-buffer buffer)
      (setq nethack-buffer-table
	    (assq-delete-all winid nethack-buffer-table)))))


;;; menus

(defun nethack-menu-mode (how)
  "Major mode for Nethack menus.

\\{nethack-menu-mode-map}"
  (setq mode-name (concat "NETHACK MENU "
			  (symbol-name how)))
  (setq major-mode 'nethack-menu-mode)
  (use-local-map nethack-menu-mode-map)
  (setq nethack-menu-how how)
  (run-hooks 'nethack-menu-mode-hook))

(defvar nethack-menu nil)

(defun nethack-menu-item-print (item)
  (let ((acc (aref item 0))
	(val (aref item 1))
	(str (aref item 2))
	(id  (aref item 3)))
    (insert (if (equal id -1)
		""
	      (concat (char-to-string acc)
		      (cond ((= val 0) " - ")
			    ((= val -1) " + ")
			    (t (format " %d " val)))))
	    str)))

(defvar nethack-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defvar nethack-window-configuration nil)

(defun nethack-menu-toggle-item (&optional count)
  "Toggle the menu item that is associated with the key event that
triggered this function call, if it is a valid option.

Does nothing if this is a pick-none menu.

Automatically submits menu if this is a pick-one menu if an option was
actually toggled."
  (interactive "P")
  (setq count
	(if (null count)
	    -1
	  (prefix-numeric-value count)))
  (if (not (eq nethack-menu-how 'pick-none))
      (let ((changed nil))
	(ewoc-map (lambda (i)
		    (when (equal (aref i 0) last-command-event)
		      (aset i 1 (if (or (= (aref i 1) 0)
					(> count -1))
				    count
				  0))
		      (setq changed t))) ; non-nil retval forces
					 ; ewoc entry redisplay
		  nethack-menu)
	(and changed
	     (eq nethack-menu-how 'pick-one)
	     (nethack-menu-submit)))))

(defun nethack-menu-toggle-all-items ()
  "Toggle all menu items, only for pick-any menus."
  (interactive)
  (if (eq nethack-menu-how 'pick-any)
      (ewoc-map (lambda (i)
		  (unless (= (aref i 3) -1)
		    (aset i 1 (if (= (aref i 1) 0)
				  -1
				0))
		    t))			; redisplay this entry
		nethack-menu)))

(defun nethack-menu-goto-next ()
  "Move to the next selectable menu item."
  (interactive)
  (if nethack-menu
      (ewoc-goto-next nethack-menu 1)))

(defun nethack-menu-goto-prev ()
  "Move to the previous selectable menu item."
  (interactive)
  (if nethack-menu
      (ewoc-goto-prev nethack-menu 1)))

(defun nethack-menu-submit ()
  "Submit the selected menu options to the nethack process.

Restores the window configuration what it was before the menu was
displayed."
  (interactive)
  (if nethack-menu
      (let ((selected (ewoc-collect nethack-menu (lambda (x) (not (= 0 (aref x 1)))))))
	(setq nethack-menu nil)
	(nh-send (mapcar (lambda (x) (list (aref x 3) (aref x 1))) selected)))
    (nh-send nil))
  (set-window-configuration nethack-window-configuration))

(defun nethack-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (setq nethack-menu nil)
  (nethack-menu-submit))

;; start_menu(window) -- Start using window as a menu.  You must call
;; start_menu() before add_menu().  After calling start_menu() you may
;; not putstr() to the window.  Only windows of type NHW_MENU may be used
;; for menus.
(defun nethack-api-start-menu (winid)
  ""
  (with-current-buffer (nethack-buffer winid)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; we don't turn on nethack-menu-mode yet, since we do not yet
      ;; know "how" this menu is going to work.
      (setq nethack-menu (ewoc-create 'nethack-menu-item-print))
      (setq nethack-unassigned-accelerator-index 0))))

(defvar nethack-unassigned-accelerator-index 0
  "Index into `nethack-accelerator-chars' indicating the next
accelerator that will be used in unassigned menus.")

(defun nethack-specify-accelerator ()
  "Return the next accelerator from `nethack-accelerator-chars'
specified by `nethack-unassigned-accelerator-index'."
  (prog1
      (aref nethack-accelerator-chars
	    nethack-unassigned-accelerator-index)
    (setq nethack-unassigned-accelerator-index
	  (+ 1 nethack-unassigned-accelerator-index))))

;; add_menu(windid window, int glyph, const anything identifier, char
;; accelerator, char groupacc, int attr, char *str, boolean preselected)
;; -- Add a text line str to the given menu window.  If identifier is 0,
;; then the line cannot be selected (e.g. a title).  Otherwise,
;; identifier is the value returned if the line is selected.  Accelerator
;; is a keyboard key that can be used to select the line.  If the
;; accelerator of a selectable item is 0, the window system is free to
;; select its own accelerator.  It is up to the window-port to make the
;; accelerator visible to the user (e.g. put 'a - ' in front of str).
;; The value attr is the same as in putstr().  Glyph is an optional glyph
;; to accompany the line.  If window port cannot or does not want to
;; display it, this is OK.  If there is no glyph applicable, then this
;; value will be NO_GLYPH.  -- All accelerators should be in the range
;; [A-Za-z].  -- It is expected that callers do not mix accelerator
;; choices.  Either all selectable items have an accelerator or let the
;; window system pick them.  Don't do both.  -- Groupacc is a group
;; accelerator.  It may be any character outside of the standard
;; accelerator (see above) or a number.  If 0, the item is unaffected by
;; any group accelerator.  If this accelerator conflicts with the menu
;; command (or their user defined alises), it loses.  The menu commands
;; and aliases take care not to interfere with the default object class
;; symbols.  -- If you want this choice to be preselected when the menu
;; is displayed, set preselected to TRUE.

(defun nethack-api-add-menu (window glyph identifier accelerator groupacc attr str preselected)
  "Create a menu item out of arguments and draw it in the menu
buffer."
  (let ((acc (if (and (not (= -1 identifier))
		      (zerop accelerator))
		 (nethack-specify-accelerator)
	       accelerator)))
    (ewoc-enter-last nethack-menu
		     (vector acc
			     (if preselected -1 0)
			     (propertize str
					 'face
					 (nethack-attr-face attr))
			     identifier))))

;; end_menu(window, prompt) -- Stop adding entries to the menu and
;; flushes the window to the screen (brings to front?).  Prompt is a
;; prompt to give the user.  If prompt is NULL, no prompt will be
;; printed.  ** This probably shouldn't flush the window any more (if **
;; it ever did).  That should be select_menu's job.  -dean

(defun nethack-api-end-menu (window prompt)
  ""
  (with-current-buffer (nethack-buffer window)
    (ewoc-set-hf nethack-menu
		 prompt
		 ;; preserve footer:
		 (cdr (ewoc-get-hf nethack-menu)))
    (ewoc-refresh nethack-menu)))

;; int select_menu(windid window, int how, menu_item **selected) --
;; Return the number of items selected; 0 if none were chosen, -1 when
;; explicitly cancelled.  If items were selected, then selected is filled
;; in with an allocated array of menu_item structures, one for each
;; selected line.  The caller must free this array when done with it.
;; The 'count' field of selected is a user supplied count.  If the user
;; did not supply a count, then the count field is filled with -1
;; (meaning all).  A count of zero is equivalent to not being selected
;; and should not be in the list.  If no items were selected, then
;; selected is NULL'ed out.  How is the mode of the menu.  Three valid
;; values are PICK_NONE, PICK_ONE, and PICK_ANY, meaning: nothing is
;; selectable, only one thing is selectable, and any number valid items
;; may selected.  If how is PICK_NONE, this function should never return
;; anything but 0 or -1.  -- You may call select_menu() on a window
;; multiple times -- the menu is saved until start_menu() or
;; destroy_nhwindow() is called on the window.  -- Note that NHW_MENU
;; windows need not have select_menu() called for them. There is no way
;; of knowing whether select_menu() will be called for the window at
;; create_nhwindow() time.

(defun nethack-api-select-menu (winid how)
  "Display the menu given by WINID and put the buffer in
`nethack-menu-mode'.

Saves the current window configuration so that it can be restored when
the menu is dismissed."
  (let ((buffer (nethack-buffer winid)))
    (if buffer
	(progn
	  (setq nethack-window-configuration (current-window-configuration))
	  ;; use the window displaying the message buffer for the
	  ;; menu, if possible.
	  (let ((message-window (get-buffer-window nethack-message-buffer)))
	    (if (not message-window)
		(pop-to-buffer (nethack-buffer winid) nil t)
	      (select-window message-window)
	      (switch-to-buffer (nethack-buffer winid) t)))
	  ;; make window larger, if necessary
	  (let ((bh (window-buffer-height (selected-window)))
		(wh (- (window-height) 1)))
	    (if (> bh wh)
		(enlarge-window (- bh wh))))
	  (nethack-menu-mode how)
	  (goto-char (point-min))
	  (message "Displaying menu"))
      (error "No such winid: %d" winid))))

;; char message_menu(char let, int how, const char *mesg) --
;; tty-specific hack to allow single line context-sensitive help to
;; behave compatibly with multi-line help menus.  -- This should only be
;; called when a prompt is active; it sends `mesg' to the message window.
;; For tty, it forces a --More-- prompt and enables `let' as a viable
;; keystroke for dismissing that prompt, so that the original prompt can
;; be answered from the message line 'help menu'.  -- Return value is
;; either `let', '\0' (no selection was made), or '\033' (explicit
;; cancellation was requested).  -- Interfaces which issue prompts and
;; messages to separate windows typically won't need this functionality,
;; so can substitute genl_message_menu (windows.c) instead.

(defun nethack-api-message-menu (let- how mesg)
  ""
  'unimplemented)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; D.  Misc. Routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make_sound(???) -- To be determined later.  THIS IS CURRENTLY
;; UN-IMPLEMENTED.

;;(defun nethack-api-make-sound ()
;;  ""
;;  'unimplemented)


;; nhbell() -- Beep at user.  [This will exist at least until sounds
;; are redone, since sounds aren't attributable to windows anyway.]

(defun nethack-api-bell ()
  "Beep at user."
  (ding))


;; mark_synch() -- Don't go beyond this point in I/O on any channel
;; until all channels are caught up to here.  Can be an empty call for
;; the moment

;;(defun nethack-api-mark-synch ()
;;  ""
;;  'unimplemented)


;; wait_synch() -- Wait until all pending output is complete
;; :flush for streams goes here:.  -- May also deal with exposure events
;; etc. so that the display is OK when return from wait_synch().

(defun nethack-api-wait-synch ()
  "Does nothing."
  )

;; delay_output() -- Causes a visible delay of 50ms in the output.
;; Conceptually, this is similar to wait_synch() followed by a nap(50ms),
;; but allows asynchronous operation.

(defun nethack-api-delay-output ()
  "Sleep for 50ms."
  ;; This is the only way I can get the desired effect of a redisplay
  ;; with a short pause.  Unfortunatly, if a keypress occurs during an
  ;; "animation" we stop getting redisplays.
  (sit-for 0 50)
  ;; Tell process to continue
  (nh-send 'dummy))


;; askname() -- Ask the user for a player name.
(defun nethack-api-askname ()
  ""
  'unimplemented)


;; cliparound(x, y)-- Make sure that the user is more-or-less centered
;; on the screen if the playing area is larger than the screen.  -- This
;; function is only defined if CLIPPING is defined.

(defun nethack-api-cliparound (x y)
  " FIXME: huh? not sure what to do here..."
  )

;; number_pad(state) -- Initialize the number pad to the given state.

(defun nethack-api-number-pad (state)
  ""
  'unimplemented)


;; suspend_nhwindows(str) -- Prepare the window to be suspended.

(defun nethack-api-suspend-nhwindows (str)
  ""
  'unimplemented)


;; resume_nhwindows() -- Restore the windows after being suspended.

(defun nethack-api-resume-nhwindows ()
  ""
  'unimplemented)


;; start_screen() -- Only used on Unix tty ports, but must be declared
;; for completeness.  Sets up the tty to work in full-screen graphics
;; mode.  Look at win/tty/termcap.c for an example.  If your window-port
;; does not need this function just declare an empty function.

(defun nethack-api-start-screen ()
  ""
  'unimplemented)


;; end_screen() -- Only used on Unix tty ports, but must be declared
;; for completeness.  The complement of start_screen().

(defun nethack-api-end-screen ()
  ""
  'unimplemented)


;; outrip(winid, int) -- The tombstone code.  If you want the
;; traditional code use genl_outrip for the value and check the #if in
;; rip.c.

(defun nethack-api-outrip (window who gold message)
  ""
  (with-current-buffer (nethack-buffer window)
    (insert (concat who " -- " message) "\n")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; III.  Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following global variables are defined in decl.c and must be used by
;; the window interface to the rest of NetHack.

;;(defvar nethack-api-toplines ""
;;  "char toplines[BUFSZ] Contains the last message printed to the
;;WIN_MESSAGE window, used by Norep().")

;; winid WIN_MESSAGE, WIN_MAP, WIN_STATUS, WIN_INVEN
;;			The four standard windows.
;; (defvar nethack-api-win-message nil)
;; (defvar nethack-api-win-map nil)
;; (defvar nethack-api-win-status nil)
;; (defvar nethack-api-win-inven nil)

;; char *AE, *AS; Checked in options.c to see if we should switch to
;; DEC_GRAPHICS.  It is #ifdefed VMS and UNIX.
;; (defvar nethack-api-ae nil)
;; (defvar nethack-api-as nil)

;; int LI, CO; Set in sys/unix/ioctl.c.
;; (defvar nethack-api-li nil)
;; (defvar nethack-api-co nil)

;; The following appears to be Unix specific.  Other ports using the
;; tty window-port should also declare this variable in one of your
;; sys/*.c files.

;; (defvar nethack-api-ospeed nil
;;   " short ospeed; Set and declared in sys/unix/unixtty.c (don't know
;; about other sys files).")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IV.  New or respecified common, high level routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are not part of the interface, but mentioned here for your
;; information.

;; char display_inventory(lets, want_reply)
;; 		-- Calls a start_menu()/add_menu()/select_menu() sequence.
;; 		   It returns the item selected, or '\0' if none is selected.
;; 		   Returns '\033' if the menu was canceled.
;; raw_printf(str, ...)
;; 		-- Like raw_print(), but accepts arguments like printf().  This
;; 		   routine processes the arguments and then calls raw_print().
;; 		-- The mac version #defines error raw_printf.  I think this
;; 		   is a reasonable thing to do for most ports.
;; pline(str, ...)
;; 		-- Prints a string to WIN_MESSAGE using a printf() interface.
;; 		   It has the variants You(), Your(), Norep(), and others
;; 		   in pline.c which all use the same mechanism.  pline()
;; 		   requires the variable 'char toplines[]' be defined; Every
;; 		   putstr() on WIN_MESSAGE must copy str to toplines[] for use
;; 		   by Norep() and pline().  If the window system is not active
;; 		   (!iflags.window_inited) pline() uses raw_print().


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; V.  Game startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following is the general order in which calls from main() should be made,
;; as they relate to the window system.  The actual code may differ, but the
;; order of the calls should be the same.


;; choose_windows(DEFAULT_WINDOW_SYS) /* choose a default window system */
;; initoptions()			   /* read the resource file */
;; init_nhwindows()		   /* initialize the window system */
;; process_options(argc, argv)	   /* process command line options or equiv */
;; if(save file is present) {
;;   display_gamewindows()		   /* create & display the game windows */
;;   dorestore()			   /* restore old game; pline()s are OK */
;; } else {
;;   player_selection()		   /* select a player type using a window */
;;   display_gamewindows()		   /* create & display the game windows */
;; }
;; pline('Hello, welcome...');

;; Choose_windows() is a common routine, and calling it in main() is
;; necessary to initialize the function pointer table to _something_
;; so that calls to raw_print() will not fail.  Choose_windows()
;; should be called almost immediately upon entering main().  Look at
;; unixmain.c for an example.

;; Display_gamewindows() is a common routine that displays the three
;; standard game windows (WIN_MESSAGE, WIN_MAP, and WIN_STATUS).  It
;; is normally called just before the 'Hello, welcome' message.

;; Process_options() is currently still unique to each port.  There
;; may be need in the future to make it possible to replace this on a
;; per window-port basis.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VI.  Conventions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; init_nhwindows() is expected to display a gee-whiz banner window,
;; including the Copyright message.  It is recommended that the
;; COPYRIGHT_BANNER_A, COPYRIGHT_BANNER_B, and COPYRIGHT_BANNER_C
;; macros from patchlevel.h be used for constructing the Copyright
;; message.  COPYRIGHT_BANNER_A is a quoted string that has the
;; NetHack copyright declaration, COPYRIGHT_BANNER_B is a quoted
;; string that states who the copyright belongs to, and
;; COPYRIGHT_BANNER_C simply says 'See License for details.' Be sure
;; to #include 'patchlevel.h' to define these macros.  Using the
;; macros will prevent having to update the Copyright information in
;; each window-port prior to each release.

;; Ports (MSDOS, TOS, MAC, etc) _may_ use window-port specific
;; routines in their port specific files, _AT_THEIR_OWN_RISK_.  Since
;; 'port' and 'window-port' are orthogonal, you make your 'port' code
;; less portable by using 'window-port' specific routines.  Every
;; effort should be made to use window-port interface routines, unless
;; there is something port specific that is better suited
;; (e.g. msmsg() for MSDOS).

;; The tty window-port is contained in win/tty, the X window port is
;; contained in win/X11.  The files in these directories contain
;; _only_ window port code, and may be replaced completely by other
;; window ports.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VII.  Implementation and Multi-window support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NetHack 3.2 and higher support multiple window systems in the same
;; binary.  When writing a new window-port, you need to follow the
;; following guidelines:

;; 1) Pick a unique prefix to identify your window-port.  For example, the tty
;;    window port uses 'tty'; the X11 window-port uses 'X11'.
;; 2) When declaring your interface function, precede the function names with
;;    your unique prefix.  E.g:

;; 	void tty_init_nhwindows()
;; 	{
;; 		/* code for initializing windows in the tty port */
;; 	}

;;    When calling window functions from within your port code, we
;;    suggest calling the prefixed version to avoid unnecessary
;;    overhead.  However, you may safely call the non-prefixed version
;;    (e.g. putstr() rather than tty_putstr()) as long as you #include
;;    'hack.h'.  If you do not include hack.h and use the non-prefixed
;;    names, you will get compile or link-time errors.

;;    We also suggest declaring all functions and port-specific data
;;    with this prefix to avoid unexpected overlaps with other
;;    window-ports.  The tty and X11 ports do not currently follow
;;    this suggestion, but do use separate non-overlapping convention
;;    for naming data and internal functions.

;; 3) Declare a structure, 'struct window_procs prefix_procs', (with
;; your prefix instead of 'prefix') and fill in names of all of your
;; interface functions.  The first entry in this structure is the name
;; of your window-port, which should be the prefix.  The other entries
;; are the function addresses.

;;    Assuming that you followed the convention in (2), you can safely
;;    copy the structure definition from an existing window-port and
;;    just change the prefixes.  That will guarantee that you get the
;;    order of your initializations correct (not all compilers will
;;    catch out-of-order function pointer declarations).

;; 4) Add a #define to config.h identifying your window-port in the
;; 'Windowing systems' section.  Follow the 'prefix_GRAPHICS'
;; convention for your window-port.

;; 5) Add your prefix to the list of valid prefixes listed in the
;; 'Known systems are' comment.

;; 6) Edit makedefs.c and add a string for your windowing system to
;; window_opts inside an #ifdef prefix_GRAPHICS.

;; 7) Edit windows.c and add an external reference to your
;; prefix_procs inside an #ifdef prefix_GRAPHICS.  Also add an entry
;; to the win_choices structure for your window-port of the form:

;;     #ifdef prefix_GRAPHICS
;; 	{ &prefix_procs, prefix_init_function },
;;     #endif

;;    The init_function is necessary for some compilers and systems to
;;    force correct linking.  If your system does not need such
;;    massaging, you may put a null pointer here.

;;    You should declare prefix_procs and prefix_init_function as
;;    extern's in your win*.h file, and #include that file at the
;;    beginning of windows.c, also inside an #ifdef prefix_GRAPHICS.
;;    Some win*.h files are rather sensitive, and you might have to
;;    duplicate your prefix_procs and prefix_init_function's instead
;;    of including win*.h.  The tty port includes wintty.h, the X11
;;    port duplicates the declarations.

;; 8) If your port uses Makefile.src, add the .c and .o files and an
;; appropriate comment in the section on 'WINSRC' and 'WINOBJ'.  See
;; Makefile.src for the style to use.  If you don't use Makefile.src,
;; we suggest using a similar convention for the make-equivalent used
;; on your system.  Also add your new source and binaries to WINSRC
;; and WINOBJ (if you want the NetHack binary to include them, that
;; is).

;; 9) Look at your port's portmain.c (the file containing main()) and
;; make sure that all of the calls match the the requirements laid out
;; in Section V.

;; Now, proceed with compilation and installation as usual.  Don't
;; forget to edit Makefile.src (or its equivalent) and config.h to set
;; the window-ports you want in your binary, the default window-port
;; to use, and the .o's needed to build a valid game.

;; One caveat.  Unfortunately, if you incorrectly specify the
;; DEFAULT_WINDOW_SYS, NetHack will dump core (or whatever) without
;; printing any message, because raw_print() cannot function without
;; first setting the window-port.

(provide 'nethack-api)

;;; nethack-api.el ends here
