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
(require 'ebmm-associations)
(require 'ebmm-view)

(defgroup ebmm-uml '()
  "Settings related to UML views for the Enterprise Business Motivation Model."
  :prefix "ebmm-uml-"
  :group 'ebmm)

;;;; Compiler Declarations
;; Defined in ebmm.el
(defvar ebmm-mode)

;; Defined externally
(declare-function plantuml-mode "plantuml-mode")

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

(defun ebmm-uml-view-extra ()
  "Generate list of strings representing extra uml options."
  (with-slots (name) ebmm-mode
    (pcase ebmm-uml-view-extra
      ((pred functionp)
       (funcall ebmm-uml-view-extra name))
      ((pred (seq-every-p #'stringp))
       ebmm-uml-view-extra))))

;;;; Methods
(cl-defmethod ebmm-view-generate ((view ebmm-viewpoint)
				  &context ((cl-type-of ebmm-mode)
					    (subclass ebmm-viewpoint)))
  "Print a VIEW by listing its associated elements via uml."
  (if (eql view ebmm-mode)
      (with-slots (name value filters) view
	(let ((buffer (get-buffer-create (format "*EBMM %s*" name)))
	      (ebmm-view-filter-functions
	       (seq-uniq (append ebmm-view-filter-functions filters)))
	      (ebmm-associations-alist ebmm-associations-alist)
	      (ebmm-uml-view-style ebmm-uml-view-style))
	  (with-current-buffer buffer
	    (or (eql major-mode 'plantuml-mode)
		(and (featurep (seq-find (apply-partially #'seq-contains-p features)
				     '(plantuml-mode plantuml-mode-autoloads)))
		 (plantuml-mode)))
	    (ebmm-mode view)
	    (erase-buffer)
	    (when
		(run-hook-with-args-until-failure
		 'ebmm-view-filter-functions
		 value)
	      (insert "@startuml\n\n' Classes\n")
	      (save-excursion
		(insert (ebmm-uml-print-element-classes))
		(insert (string-join
			 `("\n"
			   ,@ebmm-uml-view-style
			   "\n")
			 "\n"))
		(insert "' Relationships\n")
		(insert
		 (string-join
		  (seq-mapcat (pcase-lambda
				((app (ebmm-associations _ nil) (map to)))
				to)
			      value)
		  "\n"))
		(insert (string-join
			 `("\n" "' Extra"
			   ,@(ebmm-uml-view-extra)
			   "\n@enduml")
			 "\n")))))
	  (display-buffer buffer)))))

(provide 'ebmm-uml)
;;; ebmm-uml.el ends here
