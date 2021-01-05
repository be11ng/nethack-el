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
             ((string-prefix-p "OPTIONS=" elem)
              (setq result
                    (append result (nethack-options-parse-option elem))))
             ;; ((string-prefix-p "AUTOPICKUP_EXCEPTION=" elem))
             ;; ((string-prefix-p "MENUCOLOR=" elem))
             ;; ((string-prefix-p "BOLDER=" elem))
             ;; ((string-prefix-p "MSGTYPE=" elem))
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
   'nethack-options-parse-option-1
   (split-string (string-trim-left elem "[a-zA-Z]+=") "," t)))

(defun nethack-options-parse-option-1 (elem)
  ;; Cut out whitespace
  (setq elem (string-trim-left elem))
  ;; TODO: Make this more robust so if we also parse MENUCOLOR settings, then
  ;; the ":" in the regexp won't confuse this
  (if (string-match-p ":" elem)
      ;; TODO: Set this up so it auto parses things like "hilite_status"
      (split-string elem ":")
    elem))


(provide 'nethack-options)

;;; nethack-options.el ends here
