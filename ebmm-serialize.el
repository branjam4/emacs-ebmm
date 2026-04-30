;;; ebmm-serialize.el --- Serialize ebmm elements in Emacs -*- lexical-binding: t -*-

;; Author: Brandon Ellington

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
;; those objects.  This library focuses on serialization of elements.


;;; Code:
(require 'ebmm-elements)

(defun ebmm-serialize-name-to-class (name)
  "Remove spaces in NAME, replacing with a hyphen or alternate punctuation."
  (thread-last name
	       (string-replace " / " "/")
	       (string-replace " " "-")
	       downcase
	       (format "ebmm-%s")))

(defun ebmm-serialize-name-to-class-intern (name)
  "Remove spaces in NAME, and intern."
  (intern (ebmm-serialize-name-to-class name)))

(defun ebmm-serialize-class-name-to-string (class)
  "Remove ebmm prefix in CLASS, a class symbol."
  (string-trim (symbol-name class) "ebmm-"))

(defun ebmm-serialize-class-name-sans-prefix-intern (class)
  "Remove the package prefix from CLASS, a class symbol, then intern."
  (intern (ebmm-serialize-class-name-to-string class)))

(defun ebmm-serialize-to-readable (ebmm-name)
  "Reproduce a readable version of EBMM-NAME."
  (plist-get (ebmm--serialize-to-readable1 ebmm-name) :name))

(defun ebmm--serialize-to-readable1 (ebmm-name)
  "Find corresponding plist in `ebmm-elements' for EBMM-NAME."
  (pcase-let* ((base-name (thread-last ebmm-name
				       (format "%s")
				       (list "ebmm-")
				       reverse
				       (apply #'string-trim)
				       (string-replace "-" " ")
				       (string-replace "/" " / ")))
	       ((app (seq-find
		      (pcase-lambda ((map :name))
			(string-collate-equalp
			 name base-name nil 'ignore-case)))
		     element)
		ebmm-elements))
    element))

(defun ebmm-serialize-to-uml-connection
    (aggregation-type &optional source-multiplicity target-multiplicity
		      source-aggregation)
  "Depending on AGGREGATION-TYPE, print a left to right uml connection string.
SOURCE-MULTIPLICITY will show up on the left if provided,
TARGET-MULTIPLICITY on the right.

If SOURCE-AGGREGATION provided, use a right to left uml connection string."
  (string-join (append (and source-multiplicity
			    (list (format "\"%s\"" source-multiplicity)))
		       (list (format
			      "%s--%s"
			      (pcase source-aggregation
				((or "shared" "Weak") "o")
				((or "composite" "Strong") "*")
				(_ ""))
			      (pcase aggregation-type
				((or "shared" "Weak") "o")
				((or "composite" "Strong") "*")
				((guard source-aggregation) "")
				("Generalization" "|>")
				(_ ">"))))
		       (and target-multiplicity
			    (list (format "\"%s\"" target-multiplicity))))
	       " "))

(defun ebmm-serialize-to-uml-class (class)
  "Print CLASS as a class in uml."
  (pcase-let
    (((app (ebmm--serialize-to-readable1)
	  (map :name
	       (:attributes
		(app (seq-map
		      (pcase-lambda
			((map
			  (:attr-name
			   (app (format "  + %s") attribute))))
			attribute))
		     attributes))))
      class))
    (format "class \"%s\" {\n%s\n}"
	    name (if attributes (string-join attributes "\n") ""))))

(provide 'ebmm-serialize)
;;; ebmm-serialize.el ends here
