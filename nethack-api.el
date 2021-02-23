;;; nethack-api.el -- Emacs interface the lisp window-port -*- lexical-binding:t -*-

;; Copyright (C) 2002,2003,2005  Ryan Yeske and Shawn Betts

;; Author: Ryan Yeske
;; Created: Sat Mar 18 11:24:02 2000
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
;; This file is the lisp side of the Nethack/C <--> Emacs/Lisp
;; interface.  This is where all the work gets done.
;;
;; Originally a translation of nethack-3.3.0/doc/window.doc
;; from the nethack src package.

;;; Code:

(require 'nethack-compat)
(require 'gamegrid)
(require 'nethack-keys)
(require 'nethack-options)

;;; Buffer handling
(defvar nh-map-buffer nil)
(defvar nh-status-buffer nil)
(defvar nh-message-buffer nil)
(defvar nh-menu-buffer-table nil
  "An alist of (DIGIT-ID . BUFFER) pairs")
(defun nh-menu-buffer (menuid)
  "Return the buffer that corresponds to the MENUID."
  (let ((buffer (cdr (assq menuid nh-menu-buffer-table))))
    (if (buffer-live-p buffer)
        buffer
      'nobuffer)))

(defvar nh-message-highlight-overlay nil
  "Overlay used to highlight new text in the message window.")

(defvar nh-raw-print-buffer-name "*nhw raw-print*"
  "Buffer name for Nethack raw-print messages.")

(defun nhapi-raw-print (str)
  (save-current-buffer
    (let ((buffer (get-buffer-create nh-raw-print-buffer-name)))
      (pop-to-buffer buffer)
      (insert str "\n"))))

(defun nhapi-raw-print-bold (str)
  (nhapi-raw-print
   (nh-propertize str 'face (nh-attr-face 'atr-bold))))

(defun nhapi-curs (x y)
  "Set the cursor in `nh-map-buffer' to X, Y."
  (with-current-buffer nh-map-buffer
    (goto-char (gamegrid-cell-offset (- x 1) y))))


;;; Status/Attribute code:b
(defun nh-propertize-attribute (attribute form)
  "Give an ATTRIBUTE the correct faces.

ATTRIBUTE is a list going attribute name, value, oldvalue, percent, and age.  An
  attribute name is a string representing either  a stat or a condition."
  (unless nethack-options-hilites
    (nethack-options-get-hilites))
  (let* ((name (nth 0 attribute))
         (new-value (nth 1 attribute))
         (old-value (nth 2 attribute))
         (percent (nth 3 attribute))
         (age (nth 4 attribute))
         (string (format form (or new-value "")))
         (face nil))
    (when (nethack-options-set-p "statushilites")
      (setq face
            ;; TODO: Make this so that things like “up” takes precedence over
            ;; “changed” work?
            (mapcan
             (lambda (func)
               ;; feeds the function new old percent age
               (apply func (cdr attribute)))
             (nethack-options-status-hilite name))))
    (if (and (equal name "strength")
             (> new-value 18))
        (cond
         ((> new-value 118)
          (setq string (prin1-to-string (- new-value 100))))
         ((= new-value 118)
          (setq string "18/**"))
         (t
          (setq string (format "18/%02d" (- new-value 18))))))
    (if (and face
             ;; FIXME Store polymorphs?
             (not (string-equal name "HD"))
             (equal new-value "0"))
        (nh-propertize string 'face face)
      string)))

;; value oldvalue percent age
(defvar nh-status-attributes nil
  "Alist of the attributes used.

Key is a symbol, the value is a list of current, old, percent, age.")

(defvar nh-status-conditions nil
  "Alist of the NetHack conditions.

See ‘nh-status-attributes’ for details on the format.")

(defun nh-reset-status-variables ()
  (setq nh-status-attributes '(("title" nil nil nil 0)
                               ("strength" 0 0 nil 0)
                               ("dexterity" 0 0 nil 0)
                               ("constitution" 0 0 nil 0)
                               ("intelligence" 0 0 nil 0)
                               ("wisdom" 0 0 nil 0)
                               ("charisma" 0 0 nil 0)
                               ("alignment" nil nil nil 0)
                               ("score" 0 0 nil 0)
                               ("carrying-capacity" nil nil nil 0)
                               ("gold" 0 0 nil 0)
                               ("power" 0 0 nil 0)
                               ("power-max" 0 0 nil 0)
                               ("experience-level" 0 0 nil 0)
                               ("armor-class" 0 0 nil 0)
                               ("HD" 0 0 nil 0)
                               ("time" 0 0 nil 0)
                               ("hunger" nil nil nil 0)
                               ("hitpoints" 0 0 nil 0)
                               ("hitpoints-max" 0 0 nil 0)
                               ("dungeon-level" nil nil nil 0)
                               ("experience" 0 0 nil 0))
        nh-status-conditions (mapcar
                              (lambda (x)
                                (cons x
                                      '(nil nil nil 0)))
                              nethack-options-cond-all)))

(defun nhapi-status-condition-update (fields)
  (let (;(new-fields (split-string fields))
        (update-field
         (lambda (field)
           (let* ((field-name (car field))
                  (old-value (cadr field)) ; current
                  (new-value (car-safe (member field-name fields))))
             (unless (equal new-value old-value)
               ;; Update oldvalue and value
               (setf (caddr field) old-value)
               (setf (cadr field) new-value)
               (setcar (cddddr field) 0)))))) ; age
    (mapc
     update-field
     nh-status-conditions)))

(defun nhapi-status-update (field new-value percent)
  (let* ((variable (assoc field nh-status-attributes))
         (old-value (cadr variable))
         (age (cadddr variable)))
    (unless (equal new-value old-value)
      ;; TODO should this be in the let?
      (setf (alist-get field nh-status-attributes)
            (list new-value
                  old-value
                  percent
                  0))
      (when (not (string-equal field "T"))
        (run-hook-with-args 'nethack-status-attribute-change-functions
                            field new-value old-value)))))

(defun nh-status-string (format)
  (mapconcat
   (lambda (ch)
     (let ((stat (nh-status-char-to-format ch)))
       (cond
        ((equal stat "condition")
         (mapconcat
          (lambda (x)
            (let ((c (nh-propertize-attribute x "%s")))
              (if (not (string-equal c ""))
                  (concat c " "))))
          nh-status-conditions
          ""))
        (stat
         (nh-propertize-attribute
          (assoc (car stat) nh-status-attributes)
          (cdr stat)))                        ; String format
        (t (char-to-string ch)))))
   format nil))

;; TODO make this part of a unified defcustom array?
(defun nh-status-char-to-format (ch)
  "Take character CH and return the format.

If CH is the character “f” for “conditions”, then the string “condition” is
  returned instead."
  (pcase ch
    (?n '("title" . "%s"))
    (?s '("strength" . "St:%s"))
    (?d '("dexterity" . "Dx:%s"))
    (?c '("constitution" . "Cn:%s"))
    (?i '("intelligence" . "In:%s"))
    (?W '("wisdom" . "Wi:%s"))
    (?C '("charisma" . "Ch:%s"))
    (?A '("alignment" . "%s"))
    (?S '("score" . "S:%s"))
    (?r '("carrying-capacity" . "%s"))
    ;; TODO see note above on gold
    (?g '("gold" . "%s"))
    (?p '("power" . "Pw:%s"))
    (?P '("power-max" . "(%s)"))
    (?e '("experience-level" . "Xp:%s"))
    (?a '("armor-class" . "AC:%s"))
    (?D '("HD" . "HD:%s"))
    (?t '("time" . "T:%s"))
    (?G '("hunger" . "%s"))
    (?h '("hitpoints" . "HP:%s"))
    (?H '("hitpoints-max" . "(%s)"))
    (?l '("dungeon-level" . "%s"))
    (?E '("experience" . "/%s"))
    (?f "condition")))

;; This is called upon from the C half, so it should be prefixed
;; “nhapi-” rather than “nh-”.
(defun nhapi-print-status ()
  ;; title value oldvalue percent age
  (setq nh-status-attributes
        (mapcar
         (lambda (attr)
           (append (butlast attr 1) (list (1+ (nth 4 attr)))))
         nh-status-attributes))
  (setq nh-status-conditions
        (mapcar
         (lambda (attr)
           (append (butlast attr 1) (list (1+ (nth 4 attr)))))
         nh-status-conditions))
  (cl-case nethack-status-style
    (:header-line
     (with-current-buffer nh-map-buffer
       (setq header-line-format
             (nh-status-string nethack-status-header-line-format))))
    (:mode-line
     (setq mode-line-format
           (nh-status-string nethack-status-mode-line-format)))
    (:map
     (with-current-buffer nh-map-buffer
       (let ((p (next-single-property-change (point-min) 'nethack-status))
             (inhibit-read-only t))
         (when p
           (delete-region p (point-max)))
         (nh-with-point
          (goto-char (point-max))
          (insert (propertize (nh-status-string nethack-status-buffer-format) 'nethack-status t))))))
    (t
     (with-current-buffer nh-status-buffer
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (nh-status-string nethack-status-buffer-format)))))))



;;; Menu code:
(defun nhapi-menu-putstr (menuid attr str)
  "On buffer associated with MENUID, insert with ATTR the STR."
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (cond (t (goto-char (point-max))
               (insert (nh-propertize str
                                      'face
                                      (nh-attr-face attr))
                       "\n"))))))

(defun nhapi-message (attr str)
  "Insert STR to `nh-message-buffer' using ATTR face.
FIXME: doesnt actually use ATTR!"
  (nh-message attr str))

(defun nhapi-message-nohistory (_attr str)
  "Display STR in echo area.

This is used when the ATR_NOHISTORY bit in a message is set."
  (message "%s" str))

(defconst nh-colors
  [nethack-black-face 		nethack-red-face
                          nethack-green-face 		nethack-brown-face
                          nethack-blue-face 		nethack-magenta-face
                          nethack-cyan-face 		nethack-gray-face
                          nethack-dark-gray-face 	nethack-orange-face
                          nethack-bright-green-face 	nethack-yellow-face
                          nethack-bright-blue-face 	nethack-bright-magenta-face
                          nethack-bright-cyan-face 	nethack-white-face]
  "Vector indexed by Nethack's color number.")

(defun nhapi-print-glyph (x y color glyph tile ch &optional special)
  "Insert glyph into `nh-map-buffer'."
  (set-buffer nh-map-buffer)
  (setq x (- x 1))			; FIXME: put this hack in C
  (let ((inhibit-read-only t))
    (if nethack-use-tiles
        (save-excursion
          (let ((buffer-read-only nil))
            (goto-char (gamegrid-cell-offset x y))
            (delete-char 1)
            (insert-image (elt nh-tile-vector tile))))
      (gamegrid-set-cell x y ch)
      ;; If the glyph is a pet then color it with the
      ;; nethack-pet-face.
      (let ((color (if (eq special 'pet)
                       'nethack-pet-face
                     (aref nh-colors color))))
        (set-text-properties (gamegrid-cell-offset x y)
                             (1+ (gamegrid-cell-offset x y))
                             `(face
                               ,color
                               glyph
                               ,glyph))))))

(defun nhapi-yn-function (ques choices default)
  ""
  (let (key)
    ;; convert string of choices to a list of ints
    (setq choices (mapcar 'nh-char-to-int
                          (string-to-list choices)))

    (if (/= default 0)
        (setq choices (cons default choices)))

    ;; Add some special keys of our own to the choices
    (setq choices (cons 13 choices))

    (setq key (nh-read-char (concat ques " ")))
    (if (> (length choices) 1)
        (while (not (member key choices))
          (setq key (nh-read-char (concat
                                   (format "(bad %d) " key)
                                   ques " ")))))
    ;; 13, 27, and 7 are abort keys
    (nh-send (if (or (= 13 key)
                     (= 27 key)
                     (= 7 key))
                 default
               key))))

(defun nhapi-ask-direction (prompt)
  "Prompt the user for a direction"
  (let ((cmd (lookup-key nh-map-mode-map
                         (nh-read-key-sequence-vector prompt))))
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

(defun nhapi-getlin (ques)
  ""
  (nh-send (condition-case nil
               (nh-read-line (concat ques " "))
             (quit ""))))

(defun nhapi-get-ext-cmd (cmd-alist)
  "Get an extended command from the user."
  (nhapi-choose-attribute "# " cmd-alist ""))

(defun nhapi-player-selection ()
  "Does nothing right now, perhaps simply indicates that the
nhapi-choose-X calls are to follow for actual
role/race/gender/align selection.")

(defun nhapi-choose-attribute (prompt alist abort)
  "Prompts user for an element from the cars of ALIST and returns the
corresponding cdr."
  (nh-send
   (if (> (length alist) 1)
       (let ((completion-ignore-case t))
         (condition-case nil
             (cdr (assoc (completing-read prompt alist nil t) alist))
           (quit abort)))
     (cdar alist))))

(defvar nh-directory nil
  "Location of the nethack directory.

This is set when the process starts by `nhapi-init-nhwindows'.
Do not edit the value of this variable.  Instead, change the value of
`nethack-program'.")

(defun nhapi-display-file (str complain)
  (let ((file (concat nh-directory str)))
    (if (file-exists-p file)
        (view-file file)
      (if complain (message "Cannot find file %s" file)))))

(defvar nh-inventory-need-update nil
  "If non-nil, at the next command prompt, update the menu.")

(defun nhapi-update-inventory ()
  ;; FIXME: properly(?) implement perm-inven
  (setq nh-inventory-need-update t))

(defun nhapi-doprev-message ()
  ""
  (cl-case nethack-message-style
    (:map
     (nh-clear-message)
     (nh-message 'atr-none nh-last-message))
    (t
     (save-selected-window
       (save-current-buffer		; is this redundant since we
                                        ; already save the selected
                                        ; window? -rcy
         (walk-windows (lambda (w)
                         (select-window w)
                         (set-buffer (window-buffer))
                         (if (eq (current-buffer) nh-message-buffer)
                             (scroll-down)))))))))

(defun nhapi-update-positionbar (features)
  ""
  )

(defun nhapi-init-nhwindows (executable &rest args)
  "This is the first function sent by the nethack process.  Does
all of the appropriate setup."
  (setq nh-directory (file-name-directory executable))
  ;; clean up old buffers
  (mapc (lambda (b) (kill-buffer (cdr b))) nh-menu-buffer-table)
  (setq nh-menu-buffer-table nil)
  (if (get-buffer nh-raw-print-buffer-name)
      (kill-buffer nh-raw-print-buffer-name)))

(defun nhapi-exit-nhwindows (str)
  "Print the message in STR to the raw print buffer."
  (nhapi-raw-print str))

(defun nhapi-create-message-window ()
  "Create the message buffer."
  (cl-case nethack-message-style
    (:map
     ;; we need to create this buffer because messages come in before
     ;; the map is set up.
     (with-current-buffer (get-buffer-create "*nethack map*")
       (let ((inhibit-read-only t))
         (insert (make-string nh-map-width 32) "\n"))))
    (t
     (with-current-buffer (get-buffer-create "*nethack message*")
       (nh-message-mode)
       (let ((inhibit-read-only t))
         (erase-buffer))
       (setq nh-message-highlight-overlay
             (make-overlay (point-max) (point-max)))
       (overlay-put nh-message-highlight-overlay
                    'face 'nethack-message-highlight-face)
       (setq nh-message-buffer (current-buffer))))))

(defun nhapi-create-status-window ()
  "Create the status buffer."
  (with-current-buffer (get-buffer-create "*nethack status*")
    (nh-status-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq nh-status-buffer (current-buffer)))))

(defun nhapi-create-map-window ()
  "Created the map buffer."
  (if (not (buffer-live-p nh-map-buffer))
      (with-current-buffer (get-buffer-create "*nethack map*")
        (nh-map-mode)
        (setq nh-map-buffer (current-buffer)))))

(defun nhapi-create-inventory-window (menuid)
  "Create the inventory window."
  (if (not (buffer-live-p (assq menuid nh-menu-buffer-table)))
      ;; Only do if that menuid doesn't exist
      (nhapi-create-menu-window menuid)))

(defun nhapi-create-menu-window (menuid)
  "Create a menu window."
  (with-current-buffer (nhapi-create-menu 'menu menuid)
    (setq buffer-read-only t)))

(defun nhapi-create-text-window (menuid)
  "Create a text window."
  ;; text windows are treated as "pick-none" menu windows
  (nhapi-create-menu 'text menuid))

(defun nhapi-create-menu (type menuid)
  "Return a newly created buffer and add it to the menu table.

The TYPE argument is legacy and serves no real purpose."
  (let* ((name (format "*%s* %d" (symbol-name type) menuid))
         (buf (get-buffer-create name)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (kill-all-local-variables))
    (push (cons menuid buf) nh-menu-buffer-table)
    buf))

(defun nhapi-clear-message ()
  "Move overlay off the last message in `nh-message-buffer'."
  (nh-clear-message))

(defconst nh-map-width 79 "Max width of the map.")
(defconst nh-map-height 22 "Max height of the map.")
(defun nhapi-clear-map ()
  "Clear the map."
  (with-current-buffer nh-map-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (if nethack-use-tiles
          (progn ;; FIXME: test to see if emacs is capable of tiles
            (require 'nethack-tiles)
            ;; initialize the map with empty tiles
            (dotimes (i nh-map-height)
              (dotimes (j nh-map-width)
                (insert-image nh-empty-tile))
              (insert (propertize "\n" 'face 'nethack-map-tile-face))))
        (setq gamegrid-use-glyphs nil)	; dont try to use gamegrid glyphs
        (let (cursor-type)		; protect from gamegrid-init clobbering
          (gamegrid-init (make-vector 256 nil)))
        (gamegrid-init-buffer nh-map-width
                              nh-map-height
                              ? )))))
(defun nhapi-block ()
  (cl-case nethack-prompt-style
    (:map
     (nh-display-message-in-map "" t)
     (nhapi-clear-message))
    (t
     ;;(nh-read-char "nethack: -- more --")
     (read-from-minibuffer "--more--")))
  (nh-send 'block-dummy))

(defvar nh-active-menu-buffer nil)
(defvar nh-menu-how nil
  "One of pick-one, pick-none, pick-any.")

(defun nhapi-display-menu (menuid)
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((window (and nh-message-buffer
                       (get-buffer-window nh-message-buffer)))
          (size (count-lines (point-min) (point-max))))
      (if (or (not window)
              (>= size (window-height window)))
          (nhapi-select-menu menuid 'pick-none)
        (nhapi-message 'atr-none
                       (buffer-substring (point-min)
                                         (- (point-max) 1)))
        (nh-send 'dummy)))))

(defun nhapi-destroy-menu (menuid)
  (save-current-buffer
    (let ((buffer (nh-menu-buffer menuid)))
      (delete-windows-on buffer nil)
      (kill-buffer buffer)
      (setq nh-menu-buffer-table
            (nh-assq-delete-all menuid nh-menu-buffer-table)))))

(defun nh-menu-mode (how)
  "Major mode for Nethack menus.

\\{nh-menu-mode-map}"
  (setq mode-name (concat "NETHACK MENU "
                          (symbol-name how)))
  (setq major-mode 'nh-menu-mode)
  (use-local-map nh-menu-mode-map)
  (setq nh-menu-how how)
  (run-hooks 'nethack-menu-mode-hook))

(defun nh-menu-toggle-item (&optional count)
  "Toggle the menu item that is associated with the key event that
triggered this function call, if it is a valid option.

Does nothing if this is a pick-none menu.

Automatically submits menu if this is a pick-one menu and an option
was actually toggled."
  (interactive "P")
  (if (not (eq nh-menu-how 'pick-none))
      (let ((case-fold-search nil)
            (old-point (point)))
        (goto-char (point-min))
        (if (re-search-forward (format "^[%c] \\([-+]\\|[0-9]+\\) .+$"
                                       last-command-event)
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
              (if (eq nh-menu-how 'pick-one)
                  (nh-menu-submit)))
          (message "No such menu option: %c" last-command-event)
          (goto-char old-point)))))

(defun nh-menu-toggle-all-items ()
  "Toggle all menu items, only for pick-any menus."
  (interactive)
  (if (eq nh-menu-how 'pick-any)
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (while (re-search-forward nh-menu-item-regexp nil t)
            (let ((value (match-string 2)))
              (if (string-equal value "-")
                  (replace-match "+" nil nil nil 2)
                (replace-match "-" nil nil nil 2))))))))

(defun nh-menu-goto-next ()
  "Move to the next selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-end-position))
    (goto-char (if (re-search-forward nh-menu-item-regexp nil t)
                   (line-beginning-position)
                 old-point))))

(defun nh-menu-goto-prev ()
  "Move to the previous selectable menu item."
  (interactive)
  (let ((old-point (point)))
    (goto-char (line-beginning-position))
    (goto-char (if (re-search-backward nh-menu-item-regexp nil t)
                   (line-beginning-position)
                 old-point))))

(defvar nh-window-configuration nil)
(defun nh-menu-submit ()
  "Submit the selected menu options to the nethack process.

Restores the window configuration what it was before the menu was
displayed."
  (interactive)
  (goto-char (point-min))
  (let ((menu-data nil))
    (while (re-search-forward nh-menu-item-regexp nil t)
      (let ((accelerator (string-to-char (match-string 1)))
            (value (match-string 2)))
        (cond ((string-equal value "+")
               (setq value -1))
              ((string-equal value "-")
               (setq value 0))
              (t (setq value (string-to-number value))))
        (if (/= value 0)
            (setq menu-data (cons (list (nh-char-to-int accelerator) value) menu-data)))))
    (nh-send menu-data)
    (and (window-configuration-p nh-window-configuration)
         (set-window-configuration nh-window-configuration))
    (setq nh-active-menu-buffer nil)))

(defun nh-menu-cancel ()
  "Dismiss a menu with out making any choices."
  (interactive)
  (let ((inhibit-read-only t))
    (goto-char (point-min))
    ;; turn off all the options
    (while (re-search-forward nh-menu-item-regexp nil t)
      (replace-match "-" nil nil nil 1)))
  (nh-menu-submit))

(defvar nh-unassigned-accelerator-index 0
  "Index into `nh-accelerator-chars' indicating the next
accelerator that will be used in unassigned menus.")

(defun nhapi-start-menu (menuid)
  ""
  (with-current-buffer (nh-menu-buffer menuid)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; we don't turn on `nh-menu-mode' yet, since we do not yet know
      ;; "how" this menu is going to work.
      (setq nh-unassigned-accelerator-index 0))))

(defun nh-specify-accelerator ()
  "Return the next accelerator from `nh-accelerator-chars' specified
by `nh-unassigned-accelerator-index'."
  (prog1
      (aref nh-accelerator-chars
            nh-unassigned-accelerator-index)
    (setq nh-unassigned-accelerator-index
          (+ 1 nh-unassigned-accelerator-index))))

(defun nhapi-add-menu (menuid glyph tile accelerator groupacc attr str preselected)
  "Create a menu item out of arguments and draw it in the menu
buffer."
  (with-current-buffer (nh-menu-buffer menuid)
    (goto-char (point-max))
    (let ((inhibit-read-only t)
          (start (point)))
      (if (= accelerator -1)
          (insert str)
        (insert (format "%c %c %s"
                        (if (eq accelerator 0)
                            (nh-specify-accelerator)
                          accelerator)
                        (if preselected ?+ ?-)
                        str)))
      (put-text-property start (point) 'face (nh-attr-face attr))
      (insert-char ?\n 1 nil)
      (if (nethack-options-set-p 'menucolors)
          (nethack-options-highlight-menu))
      (run-hooks 'nethack-add-menu-hook))))

;; FIXME: xemacs propertize bug here
(defun nhapi-end-menu (window prompt)
  ""
  (with-current-buffer (nh-menu-buffer window)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert prompt)
      (newline))))

(defun nhapi-select-menu (menuid how)
  "Display the menu given by MENUID and put the buffer in
`nh-menu-mode'.

Saves the current window configuration so that it can be restored when
the menu is dismissed."
  (let ((buffer (nh-menu-buffer menuid)))
    (if buffer
        (progn
          (setq nh-window-configuration (current-window-configuration))
          ;; Use the window displaying the message buffer for the menu
          ;; buffer, if possible.
          (let ((message-window (and nh-message-buffer
                                     (get-buffer-window nh-message-buffer))))
            (if (not message-window)
                (pop-to-buffer (nh-menu-buffer menuid) nil t)
              (select-window message-window)
              (switch-to-buffer (nh-menu-buffer menuid) t)))
          ;; make window larger, if necessary
          (let ((bh (nh-window-buffer-height (selected-window)))
                (wh (- (window-height) 1)))
            (if (> bh wh)
                (enlarge-window (- bh wh))))
          (nh-menu-mode how)
          (goto-char (point-min))
          (message "Displaying menu")
          (setq nh-active-menu-buffer buffer))
      (error "No such menuid: %d" menuid))))

(defun nhapi-restore-window-configuration ()
  "Layout the nethack windows according to the values
`nethack-status-window-height' and `nethack-message-window-height'."
  ;; By nethack 3.6.6, the nhapi-create-map-window and
  ;; nhapi-create-inventory-window are called after
  ;; nhapi-restore-window-configuration. This may be an issue within the source
  ;; and it may be possible to patch it there, but patching it here is easier.
  (nhapi-create-map-window)             ; We don't need an if, since these
  (nhapi-create-inventory-window 3)     ; already have a check for duplicates.
  (let ((window-min-height (min nethack-status-window-height
                                nethack-message-window-height))
        other-window)
    (cl-case nethack-message-style
      (:map)
      (t
       (switch-to-buffer nh-message-buffer)
       (split-window-vertically nethack-message-window-height)
       (setq other-window t)))
    (if other-window
        (switch-to-buffer-other-window nh-map-buffer)
      (switch-to-buffer nh-map-buffer))
    (cl-case nethack-status-style
      ((:map :mode-line :header-line))
      (t
       (switch-to-buffer nh-status-buffer)
       (split-window-vertically (- nethack-status-window-height))
       (switch-to-buffer nh-map-buffer)))
    (when (buffer-live-p nh-active-menu-buffer)
      (pop-to-buffer nh-active-menu-buffer))))

(defun nhapi-bell ()
  "Beep at user."
  (ding))

(defun nhapi-wait-synch ()
  "Does nothing."
  )

(defun nhapi-delay-output ()
  "Sleep for 50ms."
  ;; This is the only way I can get the desired effect of a redisplay
  ;; with a short pause.  Unfortunatly, if a keypress occurs during an
  ;; "animation" we stop getting redisplays.
  (sit-for 0 50)
  ;; Tell process to continue
  (nh-send 'dummy))

(defun nhapi-outrip (window who gold message)
  ""
  (with-current-buffer (nh-menu-buffer window)
    (insert (concat who " -- " message) "\n")))

(defun nhapi-end ()
  (message "Goodbye.")
  (run-hooks 'nethack-end-hook))

;; Options
(defvar nh-options-cbreak nil)
(defvar nh-options-dec-graphics nil)
(defvar nh-options-echo nil)
(defvar nh-options-ibm-graphics nil)
(defvar nh-options-msg-history nil)
(defvar nh-options-num-pad nil)
(defvar nh-options-news nil)
(defvar nh-options-window-inited nil)
(defvar nh-options-vision-inited nil)
(defvar nh-options-menu-tab-sep nil)
(defvar nh-options-menu-requested nil)
(defvar nh-options-num-pad-mode nil)
(defvar nh-options-purge-monsters nil)
(defvar nh-options-bouldersym nil)
(defvar nh-options-travelcc nil)
(defvar nh-options-sanity-check nil)
(defvar nh-options-mon-polycontrol nil)

(defun nhapi-options (cbreak dec-graphics echo ibm-graphics msg-history
                             num-pad news window-inited vision-inited
                             menu-tab-sep menu-requested num-pad-mode
                             purge-monsters bouldersym travelcc
                             sanity-check mon-polycontrol &rest ignore)
  (setq nh-options-cbreak cbreak)
  (setq nh-options-dec-graphics dec-graphics)
  (setq nh-options-echo echo)
  (setq nh-options-ibm-graphics ibm-graphics)
  (setq nh-options-msg-history msg-history)
  (setq nh-options-num-pad num-pad)
  (setq nh-options-news news)
  (setq nh-options-window-inited window-inited)
  (setq nh-options-vision-inited vision-inited)
  (setq nh-options-menu-tab-sep menu-tab-sep)
  (setq nh-options-menu-requested menu-requested)
  (setq nh-options-num-pad-mode num-pad-mode)
  (setq nh-options-purge-monsters purge-monsters)
  (setq nh-options-bouldersym bouldersym)
  (setq nh-options-travelcc travelcc)
  (setq nh-options-sanity-check sanity-check)
  (setq nh-options-mon-polycontrol mon-polycontrol))

(provide 'nethack-api)

;;; nethack-api.el ends here
