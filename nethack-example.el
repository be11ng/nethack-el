;;; nethack-example.el --- Sample configurations for nethack-el

;; Copyright (C) 2002  Shawn Betts and Ryan Yeske

;; Author: Shawn Betts <sabetts@vcn.bc.ca>
;; Version: $Id: nethack-example.el,v 1.3 2002/09/20 04:15:48 rcyeske Exp $
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

;;;;;;;;;;;;;;;;;;;
(defun nethack-x-timestamp-message ()
  "Add a time-stamp to every message.

Add the following to your ~/.emacs

  (add-hook 'nethack-before-print-message-hook 
	    'nethack-x-timestamp-message)"
  (insert (format "(%d) " (elt nh-status-attribute-T 0))))
;;;;;;;;;;;;;;;;;;;
(defun nethack-x-gdb ()
  "Debug running nethack process with gdb."
  (interactive)
  (gdb (format "gdb %s/nethack %d"
	       nh-directory
	       (process-id nh-proc))))

;;;;;;;;;;;;;;
(defun nethack-x-warn-low-hp (attr new old)
  "Print a message in `nh-message-buffer' when hitpoints get low.

Add the following to your ~/.emacs

(add-hook 'nethack-status-attribute-change-functions
	  'nethack-x-warn-low-hp)"
  (if (and (string-equal attr "HP")
	   (< new old)
	   (< (/ new (float (car nh-status-attribute-HPmax))) 0.20))
      (nhapi-message 'atr-blink "Hitpoints below 20%")))

;;;;;;;;;;;;;;
(provide 'nethack-example)
;;; nethack-example.el ends here
