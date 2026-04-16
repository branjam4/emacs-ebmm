;;; ebmm-class.el --- Enterprise Business Motivation Model elements in Emacs -*- lexical-binding: t -*-

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
(require 'ebmm-serialize)
(require 'eieio-base)

;;;; Class Variables
(defvar ebmm-objects nil
  "Objects for the Enterprise Business Motivation Model.")

(defvar ebmm-class-viewpoints nil
  "Viewpoints for the Enterprise Business Motivation Model, tracked in Emacs.")

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

(defclass ebmm-viewpoint (ebmm-base)
  ((tracking-symbol :initform 'ebmm-class-viewpoints)
   (eaid :documentation "Lookup a viewpoint defined in the EBMM by this ID.")
   (name :initarg :name :documentation "Name for the viewpoint.")
   (value :documentation
	  "Elements within this view.
Control how these will be displayed by optionally adding context to the
`object-print' method—for example, by printing the object in a truncated
or specialized format depending on the major and minor modes for the
view.")
   (filters :initarg :filters
	    :initform '()
	    :documentation
	    "Some relationship types are helpful in domains and taxonomies, but less
helpful when sharing a conceptual view with others.")
   (viewpoint-doc
    :initarg :viewpoint-doc
    :documentation
    "Views are instanced in this implementation, unlike elements.
Use this slot to fill in or add info on the purpose and scope of the
view."))
  "Class for viewpoints in the Enterprise Business Motivation Model.

A viewpoint, as described in ISO 42010, is a coherent intersection of
stakeholder concerns.  By understanding the viewpoints we seek to
address, business architects can focus in on a subset of concepts needed
to address them.  For each viewpoint, a subset of the entire EBMM can be
described and used.")

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

;;;;; Build classes
;;;;; Building EBMM Elements and Views
(defun ebmm-class-generate-elements ()
  "Create eieio classes from EBMM elements.
For this function, it is assumed `ebmm-elements' contains a plist
 corresponding with slots in class function `ebmm-base'.  See
 `defebmm-class' for more details."
  (ebmm-element-add-superclasses)
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

(provide 'ebmm-class)
;;; ebmm-class.el ends here
