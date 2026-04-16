;;; ebmm-view.el --- Enterprise Business Motivation Model views in Emacs -*- lexical-binding: t -*-

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
;; those objects.  This library sets up the views from the metamodel.


;;; Code:
(require 'ebmm-elements)

;;;; Custom Variables
(defcustom ebmm-view-filters-alist
  '()
  "Set filters for viewpoints."
  :type '(alist :key-type string
		:value-type (repeat function)))

;;;; Variables
(defvar ebmm-view-plist
  (let-alist ebmm-raw-alist
    (seq-keep
     (pcase-lambda (`(_ ,(map xmi:id) .
			,(map ('elements `(_ _ . ,elements))
			      properties project)))
       (if elements
	   (let-alist (car properties)
	     (append
	      (list :name .name)
	      (let-alist (car project)
		(list :eaid xmi:id
		      :created .created
		      :modified .modified))
	      (list :documentation .documentation)
	      (list :elements
		    (seq-mapcat (pcase-lambda (`(element ,(map subject)))
				  (seq-keep (pcase-lambda ((map :eaid :name))
					      (if (string= eaid subject)
						  name))
					    ebmm-elements))
				elements))))))
     .xmi:XMI.xmi:Extension.diagrams))
  "Viewpoints from the Enterprise Business Motivation Model.")

;;;; Functions
(defun ebmm-view-plist-filter-to-elements ()
  "Filter `ebmm-viewpoints' to interesting ones that contain elements."
  (seq-filter
   (pcase-lambda ((map :elements))
     elements)
   ebmm-view-plist))

(defun ebmm-view-make-viewpoints ()
  "Create eieio objects from EBMM viewpoints.
For this function, it is assumed `ebmm-viewpoints' contains a plist
 corresponding with slots in class function `ebmm-viewpoint'."
  (seq-keep
   (pcase-lambda
     ((map :name :eaid :created :modified :documentation :elements))
     (ebmm-viewpoint
      :name name :eaid eaid
      :created created :modified modified
      :value (seq-map #'ebmm-serialize-name-to-class-intern elements)
      :filters (alist-get name ebmm-view-filters-alist
			  nil nil #'string=)
      :viewpoint-doc documentation))
	    (ebmm-view-plist-filter-to-elements)))

(defun ebmm-view-set-viewpoint-filters ()
  "Set viewpoint filters based on `ebmm-viewpoint-filters-alist'."
  (interactive)
  (seq-do (pcase-lambda (`(,name . ,functions))
	    (setf (slot-value (eieio-instance-tracker-find
			       name 'name
			       'ebmm-class-viewpoints)
			      'filters)
		  functions))
	  ebmm-view-filters-alist))

(provide 'ebmm-view)
;;; ebmm-view.el ends here
