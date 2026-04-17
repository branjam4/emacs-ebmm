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
(require 'ebmm-serialize)

;;;; Custom Variables
(defcustom ebmm-view-filters-alist
  '()
  "Set filters for viewpoints."
  :type '(alist :key-type string
		:value-type (repeat function)))

(defcustom ebmm-default-viewpoint "Primary View"
  "When activating `ebmm-viewpoint-mode' with no args, use this view by default."
  :type 'string)

(defcustom ebmm-view-filter-functions
  '(("Customer Viewpoint"
     (lambda (r)
       (seq-remove
	(pcase-lambda
	  (`(,source ,target . ,(map :target-aggregation :label)))
	  (and target-aggregation
	       (eql target 'ebmm-business-model)
	       (not (eql source 'ebmm-products-and-services))))
	r))
     (lambda (r)
       (seq-uniq
	(append '((ebmm-brand-promise ebmm-value-proposition
				      :label ""
				      :source-aggregation "none"))
		r))))
    ("EITA Viewpoint"
     (lambda (r)
       (seq-uniq
	(append
	 (seq-keep
	  (lambda (relation)
	    (pcase relation
	      (`(ebmm-application ebmm-business-or-information-tool
				  . ,_rest)
	       (plist-put relation :label "is a"))))
	  (default-toplevel-value
	   'ebmm-associations-alist))
	 r))))
    ("Structural View"
     (lambda (r)
       (thread-last ebmm-uml-view-style
		    (seq-remove
		     (apply-partially #'string-match-p "^left.+direction$"))
		    (seq-map
		     (apply-partially #'string-replace "polyline" "ortho"))
		    (setf ebmm-uml-view-style)))
     (lambda (r)
       (seq-keep
	(lambda (relation)
	  (pcase relation
	    (`(ebmm-key-performance-indicator ebmm-process-metric
					      . ,_rest)
	     (plist-put relation :label "may be a"))
	    (`(ebmm-application ebmm-business-or-information-tool
				. ,_rest)
	     (plist-put relation :label "is a"))
	    (`(ebmm-business-construct-or-model ,_taxonomy
						. ,_rest)
	     relation)
	    (`(,_rules-or-policy ebmm-directive
				 . ,(map :target-aggregation))
	     relation)
	    (`(,_mission-or-vision ebmm-principle
				   . ,(map :target-aggregation))
	     (if (string= target-aggregation "Generalization")
		 relation))
	    ((app (plist-get _ :target-aggregation)
		  (pred (string= "Generalization")))
	     relation)))
	r)))
    ("Business Model Assessment Viewpoint"
     (lambda (r)
       (thread-last
	 r
	 (seq-map
	  (lambda (relation)
	    (pcase relation
	      (`(ebmm-application ebmm-business-or-information-tool
				  . ,_rest)
	       (plist-put relation :label "is a"))
	      (`(,_influences ebmm-influencer
			      . ,(map :target-aggregation))
	       (plist-put relation :label ""))
	      (`(,_judgment ebmm-business-judgment
			    . ,(map :target-aggregation))
	       (if (string= target-aggregation "Generalization")
		   (plist-put relation :label "")))
	      (_ relation))))
	 (append
	  '((ebmm-degree-of-rivalry ebmm-industry :label "")
	    (ebmm-threat-of-substitutes ebmm-industry :label "")
	    (ebmm-barriers-to-entry ebmm-industry :label "")
	    (ebmm-supplier-power ebmm-industry :label "")
	    (ebmm-buyer-power ebmm-industry :label "")))
	 seq-uniq)))
    ("Business Model Viewpoint" ebmm-view--remove-blank-composites))
  "Abnormal hook whose functions take a plist argument.
The plist is likely in service of a view in the EBMM.  A viewpoint
object adds specific filters depending on the needs of the view
stakeholders.  Try functions in order; all functions must return
non-nil, else the hook returns nil."
  :type 'hook)

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
;;;;; For view filtering
(defun ebmm-view--remove-blank-composites (relationships)
  "Remove composite aggregations in plist RELATIONSHIPS.
This changes `ebmm-element-relationship-alist', make sure it's locally
bound before changing it."
  (seq-remove
   (pcase-lambda
     (`(,source ,target . ,(map :target-aggregation :label)))
     (and (member source relationships)
	  (string-empty-p label)
	  target-aggregation))
   relationships))

;;;;; General
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

(defun ebmm-view-completing-read ()
  "Completing read for EBMM views."
  (let ((viewpoints-alist (object-assoc-list 'name ebmm-class-viewpoints)))
    (alist-get (completing-read "Choose a Viewpoint: " viewpoints-alist)
	       viewpoints-alist nil nil #'string=)))

(provide 'ebmm-view)
;;; ebmm-view.el ends here
