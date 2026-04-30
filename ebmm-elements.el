;;; ebmm-elements.el --- Enterprise Business Motivation Model elements in Emacs -*- lexical-binding: t -*-

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
;; those objects.  This library extracts elements from the model and
;; transforms them for use by other Emacs libraries.


;;; Code:
(require 'ebmm-model)
(require 'map)
(require 'subr-x)

;;;; Variables
(defvar ebmm-elements
  (let-alist ebmm-raw-alist
    (seq-keep
     (pcase-lambda (`('elements
		      ,(map xmi:idref name)
		      . ,(map times
			      properties
			      ('extendedProperties
			       `(,(map ('package_name
					(or (pred (not (seq-contains-p
							'("Base Types"
							  "Model Elements"))))
					    package_name)))))
			      attributes)))
       (and xmi:idref
	    package_name
	    (let-alist (car properties)
	      (append
	       (let-alist (car times)
		 (list :name name
		       :eaid xmi:idref
		       :type package_name
		       :created .created
		       :modified .modified))
	       (list :documentation .documentation)
	       (list :attributes
		     (if attributes
			 (seq-keep
			  (pcase-lambda
			    (`(attribute ,(map name)
					 . ,(map ('documentation
						  `(,(map value))))))
			    (if name (list :attr-name name
					   :attr-doc value)))
			  attributes)))))))
     .xmi:XMI.xmi:Extension.elements))
  "Elements from the Enterprise Business Motivation Model.")

;;;;; Superclasses
(defun ebmm-elements-generalized ()
  "Check superclasses slot for EBMM elements to define first."
  (thread-last ebmm-elements
	       (seq-mapcat (pcase-lambda ((map :superclasses))
			     superclasses))
	       seq-uniq))

(defun ebmm-elements-add-superclasses ()
  "If an element is generalizable in the EBMM, note it before creating classes.
In the element's `ebmm-elements' superclasses property, add its
generalized element."
  (let-alist ebmm-raw-alist
    (seq-keep
     (pcase-lambda
       (`('connector . ,(map source target properties)))
       (let-alist (car properties)
	 (if (string= .ea_type "Generalization")
	     (pcase-let* ((`(,(map xmi:idref))
			   source)
			  (`(target
			     ,(map ('model
				    `(model . ,(map ('name superclass))))))
			   target)
			  (element
			   (seq-find (pcase-lambda ((map :eaid))
				       (string= eaid xmi:idref))
				     ebmm-elements))
			  (superclasses (plist-get element :superclasses)))
	       (plist-put
		element
		:superclasses (seq-uniq (append superclasses (list superclass))))))))
     .xmi:XMI.xmi:Extension.connectors))
  (thread-last ebmm-elements
	       (seq-sort
		(pcase-lambda
		  ((map (:name (app (seq-position (ebmm-elements-generalized))
				    name)))
		   (map (:name (app (seq-position (ebmm-elements-generalized))
				    name2))))
		  (or (and name name2 (< name name2))
		      (not name2))))
	       (setf ebmm-elements)))

(provide 'ebmm-elements)
;;; ebmm-elements.el ends here
