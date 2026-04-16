;;; ebmm-uml.el --- Enterprise Business Motivation Model views in UML -*- lexical-binding: t -*-

;; Author: Brandon Ellington
;; Version: 0.0.1
;; Package-Requires: eieio-base
;; Homepage: nil
;; Keywords: convenience,enterprise

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Work with Enterprise Business Motivation Model objects and views on
;; those objects.  This library generates PlantUML formatted views.


;;; Code:
(require 'ebmm-elements)

;;;; Custom Variables
(defcustom ebmm-uml-view-style
  '("' Style"
    "left to right direction"
    ;; "skinparam monochrome reverse" ; in dark mode
    ;; Ortho is good for boxes, polyline is better for lines.
    "skinparam linetype polyline"
    "skinparam Padding 16"
    "hide class circle")
  "Set style for an EBMM viewpoint uml diagram.  Goes in the middle."
  :type '(repeat string))

(defcustom ebmm-uml-view-extra
  (lambda (name)
    (list (string-join
	   `("header Enterprise Business Motivation Model"
	     "v 5.0"
	     ,(user-full-name)
	     ,(format "EBMM %s" name))
	   " - ")
	  (format "title %s" name)
	  (format "footer Page intended solely for use by %s"
		  (user-full-name))))
  "Extra settings for an EBMM viewpoint uml diagram.  Goes at the end."
  :type '(choice (repeat string)
		 function))

;;;; Functions
(defun ebmm-uml-print-element-classes ()
  "Print all elements in a view as uml classes."
  (string-join
   (seq-map #'ebmm-serialize-to-uml-class
	    (slot-value ebmm-mode 'value))
	       "\n\n"))

(defun ebmm-uml-print-view (associations)
  "Print a set of ASSOCIATIONS as uml."
  (string-join
   (seq-mapcat (pcase-lambda
		 (`(to . ,associations))
		 associations)
	       associations)
	       "\n"))

(provide 'ebmm-uml)
;;; ebmm-uml.el ends here
