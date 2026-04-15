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
(require 'ebmm-elements)

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
(defvar ebmm-element-relationship-alist
  '()
  "Store relationships defined in the Enterprise Business Motivation Model.")

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
;;;;; Relationships
(defun ebmm-add-relationships ()
  "Store relationships defined in the Enterprise Business Motivation Model."
  (let-alist ebmm-raw-alist
    (seq-keep
     (pcase-lambda
       (`('connector
	  . ,(map properties
		  ('source
		   `(source
		     . ,(map ('model
			      `(,(map ('name source-name))))
			     ('type
			      `(,(map ('aggregation
				       (or
					(pred (string= "none"))
					source-aggregation))
				      ('multiplicity source-multiplicity)))))))
		  ('target
		   `(target
		     . ,(map ('model
			      `(,(map ('name target-name))))
			     ('type
			      `(,(map ('aggregation
				       (or
					(pred (string= "none"))
					target-aggregation))
				      ('multiplicity target-multiplicity)))))))
		  ('labels `(,(map mt mb rb lb)))))) ; mid-top/bot,
					; right/left-bot
       (let-alist (car properties)
	 (and (member .ea_type '("Association"
				 "Aggregation"))
	      (or (string= .ea_type "Aggregation")
		  mt source-aggregation target-aggregation)
	      (pcase-let ((`(,source-name ,target-name)
			   (if (string= .direction "Destination -> Source")
			       (list target-name source-name)
			     (list source-name target-name))))
		(add-to-list
		 'ebmm-element-relationship-alist
		 `(,(intern (ebmm-serialize-name-to-class source-name))
		   ,(intern (ebmm-serialize-name-to-class target-name))
		   ,@(seq-mapcat
		      (pcase-lambda (`(,key ,item))
			(if item `(,key ,item)))
		      (list (list :label (if mt (downcase mt)
					   ""))
			    (pcase (list .ea_type .direction)
			      ((guard target-aggregation)
			       (list :target-aggregation
				     target-aggregation))
			      ((guard source-aggregation)
			       (list :source-aggregation
				     source-aggregation))
			      (`("Aggregation"
				 "Source -> Destination")
			       (list :target-aggregation .subtype))
			      (`("Aggregation"
				 "Destination -> Source")
			       (list :source-aggregation .subtype)))
			    (list :source-multiplicity
				  source-multiplicity)
			    (list :target-multiplicity
				  target-multiplicity)))))))))
     .xmi:XMI.xmi:Extension.connectors)))

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
