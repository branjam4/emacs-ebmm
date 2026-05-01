;;; ebmm.el --- Enterprise Business Motivation Model elements in Emacs -*- lexical-binding: t -*-

;; Author: Brandon Ellington
;; Version: 0.1.0
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
(require 'ebmm-uml)

(defgroup ebmm '()
  "Settings related to the Enterprise Business Motivation Model."
  :group 'convenience
  :group 'enterprise)

;;;; Mode
(define-minor-mode ebmm-mode
  "Activate tracking Enterprise Business Motivation Model elements in Emacs.
This mode will initialize class and relationship variables if they are
nil, and in Lisp programs accepts an ARG that should represent an
object of class function `ebmm-viewpoint'."
  :lighter " EBMM-View"
  :keymap '()
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
	  ((and (pred (eieio-object-p))
		(app (eieio-object-class-name)
		     (pred (child-of-class-p 'ebmm-viewpoint))))
	   arg)
	  ((pred (stringp)) (or (eieio-instance-tracker-find
				 arg 'name
				 'ebmm-class-viewpoints)
				(eieio-instance-tracker-find
				 ebmm-view-default 'name
				 'ebmm-class-viewpoints)))
	  ((pred (symbolp)) (eieio-instance-tracker-find
			     ebmm-view-default 'name
			     'ebmm-class-viewpoints))
	  ((and (let views (length ebmm-class-viewpoints))
		(pred (not (integerp)))
		(pred (> views)))
	   (seq-elt (ebmm-elements-generalized) (1- views)))
	  (_ (or (eieio-instance-tracker-find
		  ebmm-view-default 'name
		  'ebmm-class-viewpoints)
		 ebmm-mode)))))

(define-globalized-minor-mode ebmm-global-mode ebmm-mode
  (lambda () (unless (or ebmm-mode (minibufferp)) (ebmm-mode)))
  :keymap '())

;;;; Browsing
(defun ebmm-set-view (view)
  "Set VIEW as current for `ebmm-mode'."
  (interactive (list (ebmm-view-completing-read)))
  (ebmm-mode view))

(defun ebmm-plantuml-view (&optional view)
  "Look at a generated plantuml file of an EBMM VIEW."
  (interactive (list (ebmm-view-completing-read)))
  (let ((ebmm-mode (or view ebmm-mode)))
    (ebmm-view-generate ebmm-mode)))

(defun ebmm-documentation (class)
  "Check documentation for CLASS."
  (interactive (list (ebmm-class-completing-read)))
  (describe-symbol class))

;;;###autoload
(defun ebmm-plantuml-demo (&optional view)
  "Demonstration function for appliances or containers.
Requires PlantUML for image generation.  Interactively, choose VIEW to
demo."
  (interactive (list (ebmm-view-completing-read)))
  ;; Set EBMM mode to the Business Model view non-interactively,
  ;; generate a PlantUML buffer from the elements and associations,
  ;; then open an SVG diagram in another window.
  (with-current-buffer
      (ebmm-plantuml-view (ebmm-mode (or view "Business Model View")))
    (plantuml-preview 4)))

(easy-menu-define ebmm-menu ebmm-mode-map
  "Menu for completion convenience commands."
  '("Enterprise"
    ["Browse View (in PlantUML)" ebmm-plantuml-view]
    "---"
    ("Other Commands"
     "---"
     ["Change View" ebmm-set-view]
     "---"
     ["Read Documentation for Element" ebmm-documentation])))

(provide 'ebmm)
;;; ebmm.el ends here
