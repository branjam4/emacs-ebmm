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
(require 'ebmm-class)
(require 'ebmm-view)

;;;; Mode
(define-minor-mode ebmm-mode
  "Activate tracking Enterprise Business Motivation Model elements in Emacs.
This mode will initialize class and relationship variables if they are
nil, and in Lisp programs accepts an ARG that should represent an
object of class function `ebmm-viewpoint'."
  :lighter " EBMM-View"
  (when ebmm-mode
    ;; Running this twice defines superclasses in the correct order.
    (condition-case nil (ebmm-class-generate-elements)
      (error (ebmm-class-generate-elements)))
    (unless ebmm-associations-alist
      (ebmm-associations-add-all)
      (ebmm-associations-make-generalizations))
    (or ebmm-class-viewpoints (ebmm-view-make-viewpoints)))
  (setf ebmm-mode
	(pcase arg
	  ((guard (not ebmm-mode)) nil)
	  ((and (pred (object-p))
		(app (object-class-name)
		     (pred (child-of-class-p 'ebmm-viewpoint))))
	   arg)
	  ((pred (stringp)) (or (eieio-instance-tracker-find
				 arg 'name
				 'ebmm-class-viewpoints)
				(eieio-instance-tracker-find
				 ebmm-default-viewpoint 'name
				 'ebmm-class-viewpoints)))
	  ((pred (symbolp)) (eieio-instance-tracker-find
			     ebmm-default-viewpoint 'name
			     'ebmm-class-viewpoints))
	  ((or (pred (not (integerp)))
	       (pred (> (let views (length ebmm-class-viewpoints)))))
	   (seq-elt (ebmm-generalized) (1- views)))
	  (_ (or (eieio-instance-tracker-find
		  ebmm-default-viewpoint 'name
		  'ebmm-class-viewpoints)
		 ebmm-mode)))))

(provide 'ebmm)
;;; ebmm.el ends here
