;;; enterprise-business-motivation-model-diagrams.el --- Test EBMM UML in Emacs -*- lexical-binding: t -*-

;; Author: Brandon Ellington
;; Version: 0.0.1
;; Package-Requires: ert
;; Homepage: https://wednesdaygames.com
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

;; We want to list classes and relationships.

;;; Code:
(require 'ert)
(require 'ebmm-views)

(defmacro should-expect (test expected)
  "Wrapper for `should' using `pcase' patterns to compare EXPECTED with TEST.
A literal nil value for EXPECTED with a nil result for TEST will pass
just like `should-not' (though it is less verbose to drop the EXPECTED
arg and simply use `should-not' in that case).  But any other form is
sent as is to `pcase'.

Given the above, some patterns are too flexible for robust tests.  For
example an EXPECTED arg of \"_\" would match any result for TEST,
including nil, and could cause surprising evaluations.  Or EXPECTED
could have a \"(map foo bar)\" pattern.  Unless you know your pattern
matching, it is best to use simple, stricter patterns, like a literal
string, integer, or symbol.

Return TEST instead of \"t\"."
  (declare (debug should))
  (if expected
      `(should (pcase ,test
		 ((and ,expected result)
		  (or result
		      (quote ,expected)))))
    `(should-not ,test)))

(ert-deftest ebmm-test-uml-print-element-classes ()
  "Print elements from an EBMM view as uml classes."
  (let (ebmm-mode ebmm-class-viewpoints)
    (should (setf ebmm-mode
		  (ebmm-viewpoint
		   :name "Test View" :eaid "EAID_TEST"
		   :created (current-time) :modified (current-time)
		   :value '(ebmm-architectural-risk)
		   :viewpoint-doc "This is a view used for testing.
 You should not see this outside of a testing environment.")))
    (should (ebmm-mode "Test View"))
    (should-expect (ebmm-uml-print-element-classes)
		   "class \"Architectural Risk\" {
  + RiskAmount
  + RiskType
}")))

(provide 'enterprise-business-motivation-model-diagrams)
;;; enterprise-business-motivation-model-diagrams.el ends here
