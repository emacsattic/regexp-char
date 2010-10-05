;;; regexp-char.el --- generate regular expression matching a character

;; Copyright (C) 2009 Ralph Schleicher

;; Author: Ralph Schleicher <rs@ralph-schleicher.de>
;; Keywords: extensions

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the license, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Although `regexp-opt-charset' exists, the interface is neither
;; documented nor general enough for my needs.  The `regexp-char'
;; function provided in this package solves these issues.  It also
;; turned out that `regexp-char' runs much fast for small character
;; sets.  Here's a program for measuring the run time.
;;
;;      (require 'elp)
;;      (require 'regexp-opt)
;;      (load-file "./regexp-char.elc")
;;
;;      (elp-instrument-list '(regexp-opt-charset regexp-char))
;;
;;      (with-output-to-temp-buffer "*regexp-char*"
;;        (let ((elp-use-standard-output t)
;;              (elp-sort-by-function nil))
;;          (random t)
;;          ;; Number of characters in the set.
;;          (dolist (m '(1 2 4 8 16 32 64 128 256))
;;            (princ (format "\f\nSet Size: %d\n\n" m))
;;            (elp-reset-all)
;;            ;; Number of runs.
;;            (let ((n 100000) s)
;;              (dotimes (j n)
;;                (setq s (make-string m ?\s))
;;                (dotimes (i (length s))
;;                  (aset s i (+ ?\s (random 94))))
;;                (regexp-opt-charset (string-to-list s))
;;                (regexp-char s)))
;;            (elp-results))))
;;
;;      (elp-restore-all)

;;; Code:

;;;###autoload
(defun regexp-char (char-set &optional inverse)
  "Return a regular expression matching one character from a set.

Argument CHAR-SET is a single character or a sequence of characters.
If optional argument INVERSE is non-nil, match any character that is
not an element of CHAR-SET.

Return value is always a string.

If CHAR-SET is empty, return value is the empty string iff INVERSE
is nil.  Otherwise, the return value is `\"\\\\(?:.\\\\|\\n\\\\)\"'.

See also `regexp-quote'."
  (if (not (null inverse))
      ;; Always build a character alternative.
      (when (integerp char-set)
	(setq char-set (list char-set)))
    ;; Optimize single character matches.
    (when (= (length char-set) 1)
      (setq char-set (if (listp char-set)
			 (car char-set)
		       (aref char-set 0)))
      (unless (integerp char-set)
	(signal 'wrong-type-argument (list 'integerp char-set)))))
  (if (integerp char-set)
      (regexp-quote (char-to-string char-set))
    (let (bracket caret dash other tail start end tem)
      ;; Avoid char-table overhead for small sets.
      ;; On my machine, the break even is somewhere
      ;; between 100 and 150 elements.
      (if (> (length char-set) 128)
	  (let (table)
	    (put 'table 'char-table-extra-slots 0)
	    (setq table (make-char-table 'table))
	    (mapc (lambda (char)
		    (cond ((eq char ?\])
			   (setq bracket "]"))
			  ((eq char ?^)
			   (setq caret "^"))
			  ((eq char ?-)
			   (setq dash "-"))
			  (t
			   (aset table char t))))
		  char-set)
	    (map-char-table (lambda (char value)
			      (when value
				(push char other)))
			    table))
	(mapc (lambda (char)
		(cond ((eq char ?\])
		       (setq bracket "]"))
		      ((eq char ?^)
		       (setq caret "^"))
		      ((eq char ?-)
		       (setq dash "-"))
		      ;; Ignore duplicates.
		      ((not (eq char (car other)))
		       (push char other))))
	      (sort (append char-set nil) '<)))
      ;; Optimize character ranges.  Caveat: Elements
      ;; of OTHER are in descending order.
      (setq start other
	    tail (cdr other))
      (while tail
	(if (null end)
	    (if (= (- (car start) (car tail)) 1)
		(setq end tail)
	      (setq start tail))
	  (if (= (- (car end) (car tail)) 1)
	      (setq end tail)
	    (when (> (- (car start) (car end)) 1)
	      (setq tem (cdr start))
	      (setcar tem ?-)
	      (setcdr tem end))
	    (setq start tail
		  end nil)))
	(setq tail (cdr tail)))
      (when (and end (> (- (car start) (car end)) 1))
	(setq tem (cdr start))
	(setcar tem ?-)
	(setcdr tem end))
      (setq other (nreverse other))
      ;; Return value.
      (when inverse
	(setq inverse "^"))
      (cond ((or bracket other)
	     (concat "[" inverse bracket other caret dash "]"))
	    ((or caret dash)
	     (concat "[" inverse dash caret "]"))
	    (inverse
	     "\\(?:.\\|\n\\)")
	    (t
	     ""))
      )))

(provide 'regexp-char)

;;; regexp-char.el ends here
