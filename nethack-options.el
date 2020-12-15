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


(provide 'nethack-options)

;;; nethack-options.el ends here
