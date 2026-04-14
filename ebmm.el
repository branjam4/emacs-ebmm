;;; ebmm.el --- Enterprise Business Motivation Model elements in Emacs -*- lexical-binding: t -*-

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
;; those objects.  The EBMM is a metamodel, a frame for handling many
;; smaller models at once.  The Emacs library interfacing it will not
;; do much on its own; it requires integration with business
;; artifacts.  So, for example, with `ebmm-customer-type' being one
;; of the classes, locate (or create) where customer profiles live,
;; and import notes/references into the metamodel.


;;; Code:
(require 'ebmm-model)

;;;; Class Variables
(defvar ebmm-objects nil
  "Objects for the Enterprise Business Motivation Model.")

;;;; Classes
(defclass ebmm-base (eieio-instance-tracker eieio-instance-inheritor)
  ((tracking-symbol :initform 'ebmm-objects)
   (eaid :initarg :eaid
	 :documentation "Lookup an element defined in the EBMM by this ID.")
   (created :initarg :created
	    :documentation
	    "When an element (or its instance) was first defined in the EBMM.")
   (modified :initarg :modified
	     :documentation
	     "When an element (or its instance) was last modified in the EBMM.")
   (tags :initarg :tags
	 :initform ()
	 :documentation
	 "Extra field for eieio instances of EBMM classes.
Optionally filter instances showing up in a view by tag queries.")
   (value :initarg :value
	  :initform nil
	  :documentation
	  "Value for this instance of the EBMM element.
Control how this will be displayed in views by optionally adding context
to the `ebmm-generate-view' method—for example, by printing the object in a
truncated or specialized format depending on the major and minor modes
for the view."))
  "Base class for elements in the Enterprise Business Motivation Model."
  :abstract t)

;;;; Class Generating Macro
(defmacro defebmm-class (name superclasses slots documentation)
  "Define class function NAME with SLOTS.
It depends on SUPERCLASSES and has DOCUMENTATION."
  (declare (debug defclass))
  (append
   (list 'defclass (intern (or name "ebmm-anonymous"))
	 (seq-map #'intern
		  (if (seq-find (lambda (c)
				  (ignore-errors
				    (child-of-class-p (intern-soft c)
						      'ebmm-base)))
				superclasses)
		      superclasses
		    (append (list "ebmm-base") superclasses)))
	 slots)
   (if documentation
       (list documentation)
     '())
   (list :abstract t)))

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
					(or (pred (not (seq-contains
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

(defvar ebmm-viewpoints
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
;;;;; Superclasses
(defun ebmm-generalized ()
  "Check superclasses slot for EBMM elements to define first."
  (thread-last ebmm-elements
	       (seq-mapcat (pcase-lambda ((map :superclasses))
			     superclasses))
	       seq-uniq))

(defun ebmm-add-superclasses-to-element-plist ()
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
		  ((map (:name (app (seq-position (ebmm-generalized))
				    name)))
		   (map (:name (app (seq-position (ebmm-generalized))
				    name2))))
		  (or (and name name2 (< name name2))
		      (not name2))))
	       (setf ebmm-elements)))

;;;;; Serialization
(defun ebmm-serialize-name-to-class (name)
  "Remove spaces in NAME, replacing with a hyphen or alternate punctuation."
  (thread-last name
	       (string-replace " / " "/")
	       (string-replace " " "-")
	       downcase
	       (format "ebmm-%s")))

;;;;; Build classes
;;;;; Building EBMM Elements and Views
(defun ebmm-make-element-classes ()
  "Create eieio classes from EBMM elements.
For this function, it is assumed `ebmm-elements' contains a plist
 corresponding with slots in class function `ebmm-base'.  See
 `defebmm-class' for more details."
  (ebmm-add-superclasses-to-element-plist)
  (seq-keep (pcase-lambda ((map :name :eaid
				:superclasses
				:created :modified
				:documentation
				:attributes))
	      (eval
	       `(defebmm-class ,(ebmm-serialize-name-to-class name) ; Class name
		 ;; Superclasses
		 ,(seq-map #'ebmm-serialize-name-to-class superclasses)
		 ;; Slots
		 ,(append
		   (list `(eaid :initform ,eaid)
			 `(created :initform ,created)
			 `(modified :initform ,modified))
		   (seq-keep
		    (pcase-lambda ((map :attr-name
					:attr-doc))
		      (if attr-name
			  (list
			   (intern (ebmm-serialize-name-to-class attr-name))
			   :init-arg
			   (intern
			    (format
			     ":%s"
			     (ebmm-serialize-name-to-class attr-name)))
			   :initform nil
			   :documentation attr-doc)))
		    attributes))
		 ;; Docs
		 ,documentation)))
	    ebmm-elements))

(provide 'ebmm)
;;; ebmm.el ends here
