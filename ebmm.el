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
;; artifacts.  So, for example, with `ebmm-customer-type' being once
;; of the classes, locate (or create) where customer profiles live,
;; and import notes/references into the metamodel.


;;; Code:

(provide 'ebmm)
;;; ebmm.el ends here
