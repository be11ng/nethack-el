(defpackage :nh-tiles
  (:use :cl)
  (:export txt2tiles))

(in-package :nh-tiles)

(defconstant +files+ '(#p"../nethack/win/share/monsters.txt"
			 #p"../nethack/win/share/objects.txt" 
			 #p"../nethack/win/share/other.txt"))

;; If we've read from the file too far, we can push it back
(defvar *line* nil)

(defun pop-line (stream)
  (when (null *line*)
    (push (read-line stream nil nil) *line*))
  (pop *line*))

(defun push-line (l)
  (push l *line*))

(defun read-palette (stream)
  (let (colors)
    ;; Read the colors. Stop when we've hit a line that starts with #
    (push-line (do ((c (pop-line stream) (pop-line stream)))
		   ((char-equal (char c 0) #\#) c)
		 (push c colors)))
    (nreverse colors)))

(defun read-glyph (stream)
  (let (glyph)
    ;; Read the # line
    (pop-line stream)
    ;; read the { line
    (pop-line stream)
    ;; read till we hit a } line
    (do ((line (pop-line stream) (pop-line stream)))
	((or (null line)
	     (char-equal (char line 0) #\})))
      (push line glyph))
    (nreverse glyph)))

;; ya, it's sorta hacky.
(defun gen-color (c stream)
  (let* ((ch (char c 0))
	 (rgb (read-from-string (subseq (remove #\, c) 4))))
    (format stream "\\\"~C c #~2,'0X~2,'0X~2,'0X\\\",~%" ch (first rgb) (second rgb) (third rgb))))

(defun gen-palette (colors stream)
  (loop for c in colors
	do (gen-color c stream)))

(defun gen-glyph (palette glyph stream)
  "generate a glyph and write it to STREAM"
  (format stream "~%(create-image \"/* XPM */~%")
  (format stream "static char *xpm[] = {~%")
  (format stream "/* width height ncolors chars_per_pixel */~%")
  (format stream "\\\"16 16 16 1\\\",~%")
  (format stream "/* colors */~%")
  (gen-palette palette stream)
  (format stream "/* pixels */~%")
  (loop for g on glyph
	do (format stream "\\\"~A\\\"" (subseq (car g) 2 18))
	do (if (cdr g)
	       (format stream ",~%")
	     (format stream "~%")))
  (format stream "};\" nil t)"))

(defun gen-blank-tile (stream)
  (princ "(defconst nh-empty-tile
  (create-image \"/* XPM */
static char *xpm[] = {
/* width height ncolors chars_per_pixel */
\\\"16 16 16 1\\\",
/* colors */
\\\"A c #000000\\\",
\\\"B c #00B6FF\\\",
\\\"C c #FF6C00\\\",
\\\"D c #FF0000\\\",
\\\"E c #0000FF\\\",
\\\"F c #009100\\\",
\\\"G c #6CFF00\\\",
\\\"H c #FFFF00\\\",
\\\"I c #FF00FF\\\",
\\\"J c #914700\\\",
\\\"K c #CC7900\\\",
\\\"L c #FFB691\\\",
\\\"M c #476C6C\\\",
\\\"N c #FFFFFF\\\",
\\\"O c #DADAB6\\\",
\\\"P c #6C91B6\\\",
/* pixels */
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\",
\\\"AAAAAAAAAAAAAAAA\\\"
};\" nil t))
" stream))

(defun gen-header (stream)
  (format stream ";;; This file in auto-generated with gen-tiles.lisp~%")
  (gen-blank-tile stream)
  (format stream "(defconst nh-tile-vector (vector"))

(defun gen-footer (stream)
  (format stream "))~%(provide 'nethack-glyphs)"))

(defun gen-glyphs (src out)
  (with-open-file (in src)
    (let ((palette (read-palette in)))
      (do ((g (read-glyph in) (read-glyph in)))
	  ((null g))
	(gen-glyph palette g out)))))

(defun txt2tiles ()
  ;; Reset our pop buffer
  (setf *line* nil)
  (with-open-file (out "nethack-tiles.el" :direction :output :if-exists :supersede)
    (gen-header out)
    (loop for f in +files+
	  do (gen-glyphs f out))
    (gen-footer out)))
