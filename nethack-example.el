;;; nethack-example.el --- Sample configurations for nethack-el

;; Copyright (C) 2002  Shawn Betts and Ryan Yeske

;; Author: Shawn Betts <sabetts@vcn.bc.ca>
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

;; Here are some hacks that illustrate the power of nethack and emacs
;; combined.

;;; Code:

(defun nethack-timestamp-message ()
  "Add a time-stamp to every message.

Add the following to your ~/.emacs

  (add-hook 'nethack-message-pre-print-hook 
	    'nethack-timestamp-message)"
  (insert (format "(%d) " (elt nethack-status-attribute-T 0))))

(defun nh-gdb ()
  "Debug running nethack process with gdb."
  (interactive)
  (gdb (format "gdb %s/nethack %d"
	       nethack-directory
	       (process-id nh-proc))))

(provide 'nethack-example)
;;; nethack-example.el ends here
