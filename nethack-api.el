;;; nethack-api.el -- Emacs interface the lisp window-port

;; Copyright (C) 2002  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske
;; Created: Sat Mar 18 11:24:02 2000
;; Version: $Id: nethack-api.el,v 1.77 2002/09/13 06:33:20 rcyeske Exp $
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
;; This file is the lisp side of the Nethack/C <--> Emacs/Lisp
;; interface.
;;
;; Originally a translation of nethack-3.3.0/doc/window.doc
;; from the nethack src package.

;;; Code:

(require 'gamegrid)

(defvar nethack-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

(defun nethack-api-raw-print (str)
  (save-current-buffer
    (let ((buffer (get-buffer-create nethack-raw-print-buffer-name)))
      (pop-to-buffer nethack-message-buffer)
      (delete-other-windows)
      (insert str "\n"))))

(defun nethack-api-raw-print-bold (str)
  (nethack-api-raw-print
   (nethack-propertize str 'face (nethack-attr-face 'atr-bold))))

(defun nethack-api-curs (x y)
  "Set the cursor in `nethack-map-buffer' to X, Y."
  (with-current-buffer nethack-map-buffer
   (goto-char (gamegrid-cell-offset (- x 1) y))))

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

;;; attr value old-value age
(defun nethack-print-status-string (alist attr fmt)
  (let ((new-value (cadr (assoc attr alist))))
    (if new-value
	(progn
	  (if (numberp new-value)
	      (setq new-value (number-to-string new-value)))
	  (insert (format fmt new-value))))))

(defvar nethack-status-attribute-name (list nil nil 0))
(defvar nethack-status-attribute-monster (list nil nil 0))
(defvar nethack-status-attribute-rank (list nil nil 0))
(defvar nethack-status-attribute-St (list 0 0 0))
(defvar nethack-status-attribute-Dx (list 0 0 0))
(defvar nethack-status-attribute-Co (list 0 0 0))
(defvar nethack-status-attribute-In (list 0 0 0))
(defvar nethack-status-attribute-Wi (list 0 0 0))
(defvar nethack-status-attribute-Ch (list 0 0 0))
(defvar nethack-status-attribute-Align (list nil nil 0))
(defvar nethack-status-attribute-Dungeon (list nil nil 0))
(defvar nethack-status-attribute-Dlvl (list 0 0 0))
(defvar nethack-status-attribute-$ (list 0 0 0))
(defvar nethack-status-attribute-HP (list 0 0 0))
(defvar nethack-status-attribute-HPmax (list 0 0 0))
(defvar nethack-status-attribute-PW (list 0 0 0))
(defvar nethack-status-attribute-PWmax (list 0 0 0))
(defvar nethack-status-attribute-AC (list 0 0 0))
(defvar nethack-status-attribute-Level (list 0 0 0))
(defvar nethack-status-attribute-XP (list 0 0 0))
(defvar nethack-status-attribute-HD (list 0 0 0))
(defvar nethack-status-attribute-T (list 0 0 0))
(defvar nethack-status-attribute-Score (list 0 0 0))
(defvar nethack-status-attribute-confusion (list nil nil 0))
(defvar nethack-status-attribute-hunger (list nil nil 0))
(defvar nethack-status-attribute-sick (list nil nil 0))
(defvar nethack-status-attribute-blind (list nil nil 0))
(defvar nethack-status-attribute-stunned (list nil nil 0))
(defvar nethack-status-attribute-hallucination (list nil nil 0))
(defvar nethack-status-attribute-slimed (list nil nil 0))
(defvar nethack-status-attribute-encumbrance (list nil nil 0))

(defun nethack-reset-status-variables ()
  (setq nethack-status-attribute-name (list nil nil 0)
	nethack-status-attribute-monster (list nil nil 0)
	nethack-status-attribute-rank (list nil nil 0)
	nethack-status-attribute-St (list 0 0 0)
	nethack-status-attribute-Dx (list 0 0 0)
	nethack-status-attribute-Co (list 0 0 0)
	nethack-status-attribute-In (list 0 0 0)
	nethack-status-attribute-Wi (list 0 0 0)
	nethack-status-attribute-Ch (list 0 0 0)
	nethack-status-attribute-Align (list nil nil 0)
	nethack-status-attribute-Dungeon (list nil nil 0)
	nethack-status-attribute-Dlvl (list 0 0 0)
	nethack-status-attribute-$ (list 0 0 0)
	nethack-status-attribute-HP (list 0 0 0)
	nethack-status-attribute-HPmax (list 0 0 0)
	nethack-status-attribute-PW (list 0 0 0)
	nethack-status-attribute-PWmax (list 0 0 0)
	nethack-status-attribute-AC (list 0 0 0)
	nethack-status-attribute-Level (list 0 0 0)
	nethack-status-attribute-XP (list 0 0 0)
	nethack-status-attribute-HD (list 0 0 0)
	nethack-status-attribute-T (list 0 0 0)
	nethack-status-attribute-Score (list 0 0 0)
	nethack-status-attribute-confusion (list nil nil 0)
	nethack-status-attribute-hunger (list nil nil 0)
	nethack-status-attribute-sick (list nil nil 0)
	nethack-status-attribute-blind (list nil nil 0)
	nethack-status-attribute-stunned (list nil nil 0)
	nethack-status-attribute-hallucination (list nil nil 0)
	nethack-status-attribute-slimed (list nil nil 0)
	nethack-status-attribute-encumbrance (list nil nil 0)))

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

(defun nethack-propertize-attribute (attribute &optional how)
  (let* ((new-value (car attribute))
	 (old-value (cadr attribute))
	 (age (caddr attribute))
	 (string (format "%s" (or new-value "")))
	 (face (if (<= age nethack-status-highlight-delay)
		   (cond ((numberp new-value)
			  (cond ((eq how nil)
				 (if (> new-value old-value)
				     'nethack-status-good-face
				   'nethack-status-bad-face))
				((eq how 'lower-is-better)
				 (if (> new-value old-value)
				     'nethack-status-bad-face
				   'nethack-status-good-face))))
			 ((null new-value) 
			  nil)
			 (t 'nethack-status-neutral-face)))))
    (if face
	(nethack-propertize string 'face face)
      string)))

(defvar nethack-status-attribute-change-hook nil
  "")

(defun nethack-api-update-status (status)
  ;; store the values
  (dolist (i status)
    (let* ((variable (intern (concat "nethack-status-attribute-"
				     (car i))))
	   (old-value (car (symbol-value variable)))
	   (new-value (cadr i))
	   (age (caddr (symbol-value variable))))
      (if (equal new-value old-value)
	  (set variable (list new-value 
			      (cadr (symbol-value variable))
			      (+ 1 age)))
	(set variable (list new-value
			    old-value
			    0))
	(run-hook-with-args 'nethack-status-attribute-change-hook
			    (car i)
			    new-value
			    old-value)))))

(defun nethack-print-status ()
  ;; print the values
  (with-current-buffer nethack-status-buffer
    (erase-buffer)
    (insert (format "%s the %s St:%s Dx:%s Co:%s In:%s Wi:%s Ch:%s %s\n" 
		    (nethack-propertize-attribute nethack-status-attribute-name)
		    (if (car nethack-status-attribute-monster)
			(nethack-propertize-attribute nethack-status-attribute-monster)
		      (nethack-propertize-attribute nethack-status-attribute-rank))
		    (nethack-propertize-attribute nethack-status-attribute-St)
		    (nethack-propertize-attribute nethack-status-attribute-Dx)
		    (nethack-propertize-attribute nethack-status-attribute-Co)
		    (nethack-propertize-attribute nethack-status-attribute-In)
		    (nethack-propertize-attribute nethack-status-attribute-Wi)
		    (nethack-propertize-attribute nethack-status-attribute-Ch)
		    (nethack-propertize-attribute nethack-status-attribute-Align)))
    (insert (format "%s Dlvl:%s $:%s HP:%s(%s) Pw:%s(%s) AC:%s Xp:%s/%s T:%s %s"
		    (nethack-propertize-attribute nethack-status-attribute-Dungeon)
		    (nethack-propertize-attribute nethack-status-attribute-Dlvl)
		    (nethack-propertize-attribute nethack-status-attribute-$)
		    (nethack-propertize-attribute nethack-status-attribute-HP)
		    (nethack-propertize-attribute nethack-status-attribute-HPmax)
		    (nethack-propertize-attribute nethack-status-attribute-PW)
		    (nethack-propertize-attribute nethack-status-attribute-PWmax)
		    (nethack-propertize-attribute nethack-status-attribute-AC 'lower-is-better)
		    (nethack-propertize-attribute nethack-status-attribute-Level)
		    (nethack-propertize-attribute nethack-status-attribute-XP)
		    ;; don't propertize time
		    (car nethack-status-attribute-T)
		    ;; handle all these flags together to get the spacing right
		    (mapconcat 
		     (lambda (x) (if (not (string-equal x "")) (concat x " ")))
		     (list
		      (nethack-propertize-attribute nethack-status-attribute-confusion)
		      (nethack-propertize-attribute nethack-status-attribute-hunger)
		      (nethack-propertize-attribute nethack-status-attribute-sick)
		      (nethack-propertize-attribute nethack-status-attribute-blind)
		      (nethack-propertize-attribute nethack-status-attribute-stunned)
		      (nethack-propertize-attribute nethack-status-attribute-hallucination)
		      (nethack-propertize-attribute nethack-status-attribute-slimed)
		      (nethack-propertize-attribute nethack-status-attribute-encumbrance))
		     "")))))
    
(defun nethack-api-menu-putstr (menuid attr str)
  "On buffer associated with MENUID, insert with ATTR the STR."
  (with-current-buffer (nethack-buffer menuid)
    (let ((inhibit-read-only t))
      (cond (t (goto-char (point-max))
	       (insert (nethack-propertize str 
					   'face 
					   (nethack-attr-face attr))
		       "\n"))))))

(defun nethack-api-message (attr str)
  "Insert STR to nethack-message-buffer using ATTR face. FIXME: really do ATTR"
  (with-current-buffer nethack-message-buffer
    (goto-char (point-max))
    (run-hooks 'nethack-message-pre-print-hook)
    (insert str "\n")
    ;; cover new text with highlight overlay
    (let ((start (overlay-start nethack-message-highlight-overlay)))
      (move-overlay nethack-message-highlight-overlay
		    start (point-max)))
    ;; scroll to show maximum output on all windows displaying buffer
    (let ((l (get-buffer-window-list (current-buffer))))
      (save-selected-window
	(mapc (lambda (w)
		(select-window w)
		(set-window-point w (- (point-max) 1))
		(recenter -1))
	      l)))))    

(defun nethack-attr-face (attr)
  "Return the face corresponding with ATTR."
  (intern-soft (concat "nethack-" (symbol-name attr) "-face")))

(defun nethack-api-print-glyph (x y color glyph tile ch)
  "Insert glyph into `nethack-map-buffer'."
  (set-buffer nethack-map-buffer)
  (setq x (- x 1))			; FIXME: put this hack in C
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

    (setq key (nethack-read-char (concat ques " ")))

    (if (> (length choices) 0)
	(while (not (member key all-choices))
	  (setq key (nethack-read-char (concat ques " ")))))

    (nh-send (if (= 13 key)
		 default
	       key))))

(defun nethack-api-ask-direction (prompt)
  "Prompt the user for a direction"
  (let* ((cursor-in-echo-area t)
	 (cmd (lookup-key nethack-map-mode-map
			  (vector (nethack-read-char
				   (concat prompt " "))))))
    (nh-send
     (cond ((eq cmd 'nethack-command-north) "n")
	   ((eq cmd 'nethack-command-south) "s")
	   ((eq cmd 'nethack-command-west) "w")
	   ((eq cmd 'nethack-command-east) "e")
	   ((eq cmd 'nethack-command-northwest) "nw")
	   ((eq cmd 'nethack-command-northeast) "ne")
	   ((eq cmd 'nethack-command-southwest) "sw")
	   ((eq cmd 'nethack-command-southeast) "se")
	   ((eq cmd 'nethack-command-up) "up")
	   ((eq cmd 'nethack-command-down) "down")
	   ((eq cmd 'nethack-command-rest-one-move) "self")
	   ((eq cmd 'nethack-command-search) "self")
	   (t "nowhere")))))

(defun nethack-api-getlin (ques)
  ""
  (nh-send (condition-case nil
	       (read-from-minibuffer (concat ques " "))
	     (quit ""))))

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

(defun nethack-api-display-file (str complain)
  (let ((file (concat nethack-directory str)))
    (if (file-exists-p file)
	(view-file file)
      (if complain (message "Cannot find file %s" file)))))

(defvar nethack-inventory-need-update nil
  "If non-nil, at the next command prompt, update the menu.")

(defun nethack-api-update-inventory ()
;; FIXME: properly(?) implement perm-inven
  (setq nethack-inventory-need-update t))

(defun nethack-api-doprev-message ()
  ""
  (save-selected-window
    (save-current-buffer		; is this redundant since we
					; already save the selected
					; window? -rcy
      (walk-windows (lambda (w)
		      (select-window w)
		      (set-buffer (window-buffer))
		      (if (eq (current-buffer) nethack-message-buffer)
			  (scroll-down)))))))

(defun nethack-api-update-positionbar (features)
  ""
  )

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

(defun nethack-api-exit-nhwindows (str)
  ""
  ;; print the message in STR to the raw print buffer
  (nethack-api-raw-print str))

(defvar nethack-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs")

(defvar nethack-map-buffer nil)
(defvar nethack-status-buffer nil)
(defvar nethack-message-buffer nil)

(defvar nethack-message-highlight-overlay nil
  "Overlay used to highlight new text in the message window.")

(defface nethack-message-highlight-face
  '((t (:foreground "black" :background "green")))
  "The face used to highlight new text in the message window."
  :group 'nethack-faces)

(defun nethack-api-create-message-window ()
  "Create the message buffer."
  (with-current-buffer (get-buffer-create "*nethack message*")
    (setq nethack-message-highlight-overlay
	  (make-overlay (point-max) (point-max)))
    (overlay-put nethack-message-highlight-overlay 
		 'face 'nethack-message-highlight-face)
    (setq nethack-message-buffer (current-buffer))))

(defun nethack-api-create-status-window ()
  "Create the status buffer."
  (with-current-buffer (get-buffer-create "*nethack status*")
    (setq nethack-status-buffer (current-buffer))))

(defun nethack-api-create-map-window ()
  "Created the map buffer."
  (with-current-buffer (get-buffer-create "*nethack map*")
    (nethack-map-mode)
    (setq nethack-map-buffer (current-buffer))))

(defun nethack-api-create-inventory-window (menuid)
  "Create the inventory window."
  (nethack-api-create-menu-window menuid))

(defun nethack-api-create-menu-window (menuid)
  "Create a menu window."
  (with-current-buffer (nethack-api-create-menu 'menu menuid)
    (setq buffer-read-only t)))

(defun nethack-api-create-text-window (menuid)
  "Create a text window."
  ;; text windows are treated as "pick-none" menu windows
  (nethack-api-create-menu 'text menuid))

(defun nethack-api-create-menu (type menuid)
  "Return a newly created buffer and add it to the menu table.  

The TYPE argument is legacy and serves no real purpose."
  (let* ((name (format "*%s* %d" (symbol-name type) menuid))
	 (buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables))
    (push (cons menuid buf) nethack-buffer-table)
    buf))

(defun nethack-api-clear-message ()
  "Move overlay off the last message in `nethack-message-buffer'."
  (with-current-buffer nethack-message-buffer
    (move-overlay nethack-message-highlight-overlay
		  (point-max) (point-max))))

(defun nethack-api-clear-map ()
  "Clear the map."
  (with-current-buffer nethack-map-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if nethack-use-glyphs
	  (progn ;; FIXME: test to see if emacs is capable of glyphs
	    (require 'nethack-glyphs)
	    ;; initialize the map with empty glyphs
	    (dotimes (i nethack-map-height)
	      (dotimes (j nethack-map-width)
		(insert-image nethack-empty-glyph))
	      (insert (propertize "\n" 'face 'nethack-map-glyph-face))))
	(gamegrid-init (make-vector 256 nil))
	(gamegrid-init-buffer nethack-map-width
			      nethack-map-height
			      ? )))))

(defun nethack-api-display-menu (menuid)
  (with-current-buffer (nethack-buffer menuid)
    (let ((window (get-buffer-window nethack-message-buffer))
	  (size (count-lines (point-min) (point-max))))
      (if (or (not window)
	      (>= size (window-height window)))
	  (nethack-api-select-menu menuid 'pick-none)
	(nethack-api-message 'atr-none 
			     (buffer-substring (point-min)
					       (- (point-max) 1)))
	(nh-send 'dummy)))))

(defun nethack-api-block ()
  ;;(nethack-read-char "nethack: -- more --")
  (read-from-minibuffer "--more--")
  (nh-send 'dummy))

(defcustom nethack-status-window-height 4
  "Height of the status window."
  :type '(integer)
  :group 'nethack)

(defcustom nethack-message-window-height 10
  "Height of the message window."
  :type '(integer)
  :group 'nethack)

(defun nethack-api-restore-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  (let ((window-min-height (min nethack-status-window-height
				nethack-message-window-height)))
    (delete-other-windows)
    (switch-to-buffer nethack-status-buffer)
    (split-window-vertically (- nethack-status-window-height))
    (switch-to-buffer nethack-message-buffer)
    (split-window-vertically nethack-message-window-height)
    (switch-to-buffer-other-window nethack-map-buffer)
    (if (buffer-live-p nethack-active-menu-buffer)
	(pop-to-buffer nethack-active-menu-buffer))))

(defun nethack-api-destroy-menu (menuid)
  (save-current-buffer
    (let ((buffer (nethack-buffer menuid)))
      (delete-windows-on buffer nil)
      (kill-buffer buffer)
      (setq nethack-buffer-table
	    (nethack-assq-delete-all menuid nethack-buffer-table)))))

(defun nethack-menu-mode (how)
  "Major mode for Nethack menus.

\\{nethack-menu-mode-map}"
  (setq mode-name (concat "NETHACK MENU "
			  (symbol-name how)))
  (setq major-mode 'nethack-menu-mode)
  (use-local-map nethack-menu-mode-map)
  (setq nethack-menu-how how)
  (run-hooks 'nethack-menu-mode-hook))

(defvar nethack-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defvar nethack-window-configuration nil)

(defun nethack-menu-toggle-item (&optional count)
  "Toggle the menu item that is associated with the key event that
triggered this function call, if it is a valid option.

Does nothing if this is a pick-none menu.

Automatically submits menu if this is a pick-one menu and an option
was actually toggled."
  (interactive "P")
  (if (not (eq nethack-menu-how 'pick-none))
      (let ((case-fold-search nil)
	    (old-point (point)))
	(goto-char (point-min))
	(if (re-search-forward (format "^[%c] \\([-+]\\|[0-9]+\\) .+$" 
				last-command-char)
			       nil t)
	    (let ((value (match-string 1))
		  (start (match-beginning 1))
		  (end (match-end 1))
		  (inhibit-read-only t))
	      (delete-region start end)
	      (goto-char start)
	      (if (and count)
		  (insert (number-to-string (if (consp count)
						(car count)
					      count)))
		(if (string-equal value "-")
		    (insert "+")
		  (insert "-")))
	      (beginning-of-line)
	      (if (eq nethack-menu-how 'pick-one)
		  (nethack-menu-submit)))
	  (message "No such menu option: %c" last-command-char)
	  (goto-char old-point)))))
	  
(defun nethack-menu-toggle-all-items ()
  "Toggle all menu items, only for pick-any menus."
  (interactive)
  (if (eq nethack-menu-how 'pick-any)
      (save-excursion
	(let ((inhibit-read-only t))
	  (goto-char (point-min))
	  (while (re-search-forward "^[a-zA-Z*] \\([-+]\\|[0-9]+\\) .+$" nil t)
	    (let ((value (match-string 1)))
	      (if (string-equal value "-")
		  (replace-match "+" nil nil nil 1)
		(replace-match "-" nil nil nil 1))))))))

(defun nethack-menu-goto-next ()
  "Move to the next selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-end-position))
    (goto-char (if (re-search-forward "^[a-zA-Z*] [-+]\\|[0-9]+ .+$" nil t)
		   (line-beginning-position)
		 old-point))))

(defun nethack-menu-goto-prev ()
  "Move to the previous selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-beginning-position))
    (goto-char (if (re-search-backward "^[a-zA-Z*] [-+]\\|[0-9]+ .+$" nil t)
		   (line-beginning-position)
		 old-point))))

(defun nethack-menu-submit ()
  "Submit the selected menu options to the nethack process.

Restores the window configuration what it was before the menu was
displayed."
  (interactive)
  (goto-char (point-min))
  (let ((menu-data nil))
    (while (re-search-forward "^\\([a-zA-Z*]\\) \\([-+]\\|[0-9]+\\) .+$" nil t)
      (let ((accelerator (string-to-char (match-string 1)))
	    (value (match-string 2)))
	(cond ((string-equal value "+")
	       (setq value -1))
	      ((string-equal value "-")
	       (setq value 0))
	      (t (setq value (string-to-number value))))
	(if (/= value 0)
	    (setq menu-data (cons (list (nethack-char-to-int accelerator) value) menu-data)))))
    (nh-send menu-data)
    (and (window-configuration-p nethack-window-configuration)
	 (set-window-configuration nethack-window-configuration))
    (setq nethack-active-menu-buffer nil)
    (message "%S" menu-data)))
	
(defun nethack-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    ;; turn off all the options
    (while (re-search-forward "^[a-zA-Z*] \\([-+]\\|[0-9]+\\) .+$" nil t)
      (replace-match "-" nil nil nil 1)))
  (nethack-menu-submit))

(defun nethack-api-start-menu (menuid)
  ""
  (with-current-buffer (nethack-buffer menuid)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; we don't turn on nethack-menu-mode yet, since we do not yet
      ;; know "how" this menu is going to work.
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

(defun nethack-api-add-menu (menuid glyph tile accelerator groupacc attr str preselected)
  "Create a menu item out of arguments and draw it in the menu
buffer."
  (with-current-buffer (nethack-buffer menuid)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
	  (start (point)))
      (if (= accelerator -1)
	  (insert str)
	(insert (format "%c %c %s"
			(if (eq accelerator 0)
			    (nethack-specify-accelerator)
			  accelerator)
			(if preselected ?+ ?-)
			str)))
      (put-text-property start (point) 'face (nethack-attr-face attr))
      (insert-char ?\n 1 nil))))

(defun nethack-api-end-menu (window prompt)
  ""
  (with-current-buffer (nethack-buffer window)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert prompt)
      (newline))))

(defvar nethack-active-menu-buffer nil)

(defun nethack-api-select-menu (menuid how)
  "Display the menu given by MENUID and put the buffer in
`nethack-menu-mode'.

Saves the current window configuration so that it can be restored when
the menu is dismissed."
  (let ((buffer (nethack-buffer menuid)))
    (if buffer
	(progn
	  (setq nethack-window-configuration (current-window-configuration))
	  ;; use the window displaying the message buffer for the
	  ;; menu, if possible.
	  (let ((message-window (get-buffer-window nethack-message-buffer)))
	    (if (not message-window)
		(pop-to-buffer (nethack-buffer menuid) nil t)
	      (select-window message-window)
	      (switch-to-buffer (nethack-buffer menuid) t)))
	  ;; make window larger, if necessary
	  (let ((bh (nethack-window-buffer-height (selected-window)))
		(wh (- (window-height) 1)))
	    (if (> bh wh)
		(enlarge-window (- bh wh))))
	  (nethack-menu-mode how)
	  (goto-char (point-min))
	  (message "Displaying menu")
	  (setq nethack-active-menu-buffer buffer))
      (error "No such menuid: %d" menuid))))

(defun nethack-api-bell ()
  "Beep at user."
  (ding))

(defun nethack-api-wait-synch ()
  "Does nothing."
  )

(defun nethack-api-delay-output ()
  "Sleep for 50ms."
  ;; This is the only way I can get the desired effect of a redisplay
  ;; with a short pause.  Unfortunatly, if a keypress occurs during an
  ;; "animation" we stop getting redisplays.
  (sit-for 0 50)
  ;; Tell process to continue
  (nh-send 'dummy))

(defun nethack-api-outrip (window who gold message)
  ""
  (with-current-buffer (nethack-buffer window)
    (insert (concat who " -- " message) "\n")))

(defun nethack-api-end ()
  (message "Goodbye."))

(provide 'nethack-api)

;;; nethack-api.el ends here
