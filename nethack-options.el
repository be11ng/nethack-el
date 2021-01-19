;;; nethack-options.el --- Options parser for nethack-el -*- lexical-binding:t -*-

;; Copyright (C) 2020  Benjamin Yang

;; Author: Benjamin Yang <be11ng@protonmail.com>
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

;; This file helps nethack-el read a user's options file and sets the right
;; variables.  This is done in an attempt to make things a little less reliant
;; on the C half of nethack-el.

;; I feel like it would be a bad idea to replace the options file altogether the
;; way it is currently written --- a bunch of the map-drawing stuff isn't
;; abstracted away but is actually writing a character to a place.  This would
;; be bad, say, if someone wanted to make a boulder a `0` or something like
;; that.  Far easier to just look for options that might screw things up (like
;; IBM graphics) and adjust for them as necessary.  At the very least, writing
;; this lays the groundwork in case the options file does need to be read in
;; entirety.

;;; Code:

(defcustom nethack-options-file
  ;; On windows this would be ‘\%USERPROFILE%\NetHack\3.6’, but I'm too lazy to
  ;; figure out how to get this to work between OSs.
  (expand-file-name "~/.nethackrc")
  "The nethack configuration file.

The NetHack executable will be called with this in mind.  Some minimal options
parsing is also done on the Lisp-side of nethack-el."
  :type '(file)
  :group 'nethack)

(defcustom nethack-options
  (nethack-options-parse)
  "Alist representing nethackrc options

This can be edited through ‘custom-set’."
  ;; TODO implement customize better
  ;; :type '(alist :key-type string)
  :type '(sexp)
  :group 'nethack)



(defconst nethack-options-fields
  '("title" "dungeon-level" "experience-level" "strength" "gold" "experience"
  "dexterity" "hitpoints" "HD" "constitution" "hitpoints-max" "time"
  "intelligence" "power" "hunger" "wisdom" "power-max" "carrying-capacity"
  "charisma" "armor-class" "condition" "alignment" "score" "characteristics"))

(defconst nethack-options-behaviors
  '(always up down changed)
  ;; Need text-match, absolute value, and percentage
  )

(defun nethack-options-status-field-p (field)
  (member
   (if (symbolp field)
       (symbol-name field)
     field)
   nethack-options-fields))



(defun nethack-options-parse ()
  "Return a parsed list of ‘nethack-options-file’.

Maybe I should have used eieio."
  (when (file-exists-p nethack-options-file)
    (let (result elem)
      (with-temp-buffer
        (insert-file-contents nethack-options-file)
        (while (not (eobp))
          (narrow-to-region (point) (point-at-eol))
          ;; Skip blank lines and comments
          (unless (or (eobp)
                      (eq (char-after) ?#))
            (setq elem (buffer-string))
            (cond
             ;; Matches both OPTION= and OPTIONS=
             ((string-prefix-p "OPTION" elem)
              (setq result
                    (append result (nethack-options-parse-option elem))))
             ;; ((string-prefix-p "AUTOPICKUP_EXCEPTION=" elem))
             ((string-prefix-p "MENUCOLOR=" elem)
              (setq result
                    (append result (list
                                    (nethack-options-parse-menucolor elem)))))
             ;; ((string-prefix-p "BOLDER=" elem))
             ;; ((string-prefix-p "MSGTYPE=" elem))
             ;; ((string-prefix-p "BINDINGS=" elem))
             ;; ((string-prefix-p "SOUND=" elem))
             ;; I'm probably not going to process “CHOOSE” lines
             ))
          (widen)
          (forward-line 1)))
      result)))

(defun nethack-options-parse-option (elem)
  "Parse a nethackrc OPTIONS= line.

Returns a list of the options set."
  ;; (when (string-prefix-p "OPTIONS=" elem)
  ;;   (setq elem (string-trim-left elem "[a-zA-Z]+=")))
  (mapcar
   #'nethack-options-parse-option-1
   (split-string (string-trim-left elem "[a-zA-Z]+=") "," t)))

(defun nethack-options-parse-option-1 (elem)
  ;; Cut out whitespace
  (setq elem (string-trim-left elem))
  ;; TODO: Make this more robust so if we also parse MENUCOLOR settings, then
  ;; the ":" in the regexp won't confuse this
  (if (string-match-p ":" elem)
      ;; TODO: Set this up so it auto parses things like "hilite_status"
      ;; The string trim regexp is copied from ‘string-trim-left’.
      (let* ((option
              (split-string elem ":" t "[ \t\n\r]+"))
             (op (car option))
             (params (cadr option)))
        (cons
         op
         (cond
          ((equal "hilite_status" op)
           (nethack-options-parse-hilite-status params))
          (t
           (list params)))))
    elem))

(defun nethack-options-parse-hilite-status (params)
  (let* ((ops (split-string params "/" t "[ \t\n\r]+"))
         (field-name (pop ops))
         (result (list field-name)))
    (while ops
      (setq result
            (cons
             (cond
              ;; These two need to be handled a little differently
              ((equal "condition" (car ops))
               (list (cons 'condition
                           (pop ops))   ; (cadr ops)
                     (nethack-options-parse-attr (pop ops))))
              ((string-suffix-p "-max" field-name)
               (list (nethack-options-parse-attr (pop ops))))
              ;; In most cases:
              ((and (nethack-options-status-field-p field-name)
                    (cdr ops))
               (list (nethack-options-parse-status-behav (pop ops))
                     (nethack-options-parse-attr (pop ops))))
              ;; For something like: hilite_status:hitpoints/<=30%/red/normal
              (t
               (list '(else)
                     (nethack-options-parse-attr (pop ops)))))
             result)))
    (reverse result)))

(defun nethack-options-parse-status-behav (behav)
  (list 'behavior
        behav))

(defun nethack-options-parse-attr (attributes)
  (list 'attributes
        (if (string-match-p "[&+]" attributes)
            (split-string attributes "[&+]" t "[ \t\n\r]+")
          attributes)))

(defun nethack-options-parse-menucolor (elem)
  "Parse a nethackrc MENUCOLOR= line.

Returns an alist entry of the options set."
  (when (string-prefix-p "MENUCOLOR=" elem)
    (setq elem (string-trim-left elem "[a-zA-Z]+=")))
  ;; In case the regexp contains “=”
  ;; TODO: Do this without using ‘reverse’
  (setq elem (reverse (split-string elem "=" t)))
  (setq attr (pop elem))
  (if (stringp elem)
      (setq elem (apply #'concat (reverse elem))))
  (setq elem (car elem))                ; There should only be 1 item
  ;; Remove quotes from either side of ‘elem’
  (setq elem (substring elem 1 -1))
  (list 'menucolor
        elem
        (nethack-options-parse-attr attr)))



(defun nethack-options-set-p (op)
  "Check if OP is set as a NetHack option.

This only really makes sense to use it on options that don't take arguments (so
don't use it to theck “pickup types”, but do check things like “showexp”).
It can check these options, though it doesn't make sense to."
  ;; There's probably a more efficient way to do this through a ‘filter’ or
  ;; something of that like but this works.
  (when (symbolp op)
    (setq op (symbol-name op)))
  (setq result
        (or (member op nethack-options)
            (assoc op nethack-options)))
  (if (and result
           (stringp (car result)))
      (car result)
    result))


(provide 'nethack-options)

;;; nethack-options.el ends here
