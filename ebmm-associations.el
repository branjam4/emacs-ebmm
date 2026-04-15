;;; ebmm-associations.el --- Enterprise Business Motivation Model relationships -*- lexical-binding: t -*-

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
;; those objects.  This library focuses on relationships between
;; elements.


;;; Code:
(require 'ebmm-serialize)

;;;; Variables
(defvar ebmm-associations-alist
  '()
  "Store relationships defined in the Enterprise Business Motivation Model.")

;;;; Functions
;;;;; Relationships
(defun ebmm-associations-add-all ()
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
		 'ebmm-associations-alist
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

(defun ebmm-associations-get-all (element)
  "List all associations of ELEMENT, a symbol."
  (thread-last
    ebmm-associations-alist
    (seq-filter
     (pcase-lambda (`(,(app (eql element) source)
		      ,(app (eql element) target)))
       (or source target)))
    (seq-group-by
     (pcase-lambda (`(,(app (eql element) source)
		      ,(app (eql element) target)))
       (if source 'to 'from)))
    (seq-map (pcase-lambda (`(,direction . ,relationships))
	      (cons direction
		    (seq-keep
		     (pcase-lambda (`(,source ,target
					      . ,(map :label
						      :source-aggregation
						      :target-aggregation
						      :source-multiplicity
						      :target-multiplicity)))
		       (if (and source target label)
			   (format "\"%s\" %s \"%s\"%s"
				   (ebmm-serialize-to-readable source)
				   (ebmm-serialize-to-uml-connection
				    target-aggregation
				    source-multiplicity target-multiplicity
				    source-aggregation)
				   (ebmm-serialize-to-readable target)
				   (if (string-empty-p label) ""
				     (format " : %s" label)))))
		     relationships))))))

;;;; Methods
;;;;; Relationship
(cl-defmethod ebmm-associations ((ele-type1 string) (ele-type2 string))
  "Check associations between ELE-TYPE1 and ELE-TYPE2."
  (pcase ebmm-associations-alist
    ((and (app (alist-get (intern ele-type1)) source)
	  (let target (alist-get (intern ele-type2) source))
	  (guard target))
     (format "%s %s %s" ele-type1 target ele-type2))
    ((and (app (alist-get (intern ele-type2)) source)
	  (let target (alist-get (intern ele-type1) source))
	  (guard target))
     (format "%s %s %s" ele-type2 target ele-type1))
    (_ nil)))

(cl-defmethod ebmm-associations ((ele-type1 (subclass ebmm-base))
				 (ele-type2 (subclass ebmm-base)))
  "Check associations between ELE-TYPE1 and ELE-TYPE2."
  (ebmm-associations (ebmm-serialize-class-name-to-string ele-type1)
		     (ebmm-serialize-class-name-to-string ele-type2)))

(cl-defmethod ebmm-associations ((element ebmm-base) &rest _)
  "List all associations of ELEMENT, an object."
  (ebmm-associations-get-all
   (intern (ebmm-serialize-class-name-to-string
	    (object-class-name element)))))

(cl-defmethod ebmm-associations ((ele-type (subclass ebmm-base)) _
				 &context ((cl-type-of ebmm-viewpoint-mode)
					   (subclass ebmm-viewpoint)))
  "List all associations of ELE-TYPE."
  (ebmm-associations-get-all ele-type))

(cl-defmethod ebmm-associations ((ele-type (subclass ebmm-base))
				 (_view ebmm-viewpoint))
  "List all associations of ELE-TYPE."
  (ebmm-associations-get-all ele-type))

(provide 'ebmm-associations)
;;; ebmm-associations.el ends here
