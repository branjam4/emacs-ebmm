;;; enterprise-business-motivation-model-diagrams.el --- Test EBMM UML in Emacs -*- lexical-binding: t -*-

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

;; We want to list classes and relationships.

;;; Code:
(require 'ert)
(require 'ebmm)

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

(ert-deftest ebmm-test-uml-print-view ()
    "Print a diagram derived from an Enterprise Business Motivation Model view.
The view contains a list of elements.  Those elements have associations
in `ebmm-element-relationship-alist'.  Only associations where both
elements appear in the view should show up in the diagram."
  (let (ebmm-mode ebmm-class-viewpoints)
    (ebmm-viewpoint
     :name "Test View" :eaid "EAID_TEST"
     :created (current-time) :modified (current-time)
     :value '(ebmm-business-strategy
	      ebmm-business-initiative/program
	      ebmm-business-model
	      ebmm-influencer
	      ebmm-objective
	      ebmm-mission
	      ebmm-vision)
     :viewpoint-doc "This is a view used for testing. You should not see this outside of a
testing environment.")
    (ebmm-mode "Test View")
    (should-expect (cl-type-of ebmm-mode) 'ebmm-viewpoint)
    (should (ebmm-serialize-to-readable "ebmm-business-construct-or-model"))
    (should-expect (ebmm-serialize-to-readable "ebmm-use-case/user-story")
		   "Use Case / User Story")
    (should-expect (ebmm-uml-print-view
		    (ebmm-associations 'ebmm-mission nil))
		   "\"Mission\" --> \"Vision\" : makes operative the")
    (should-expect
     (string-join
      (seq-sort
       #'string<
       (string-lines
	(ebmm-uml-print-view
	 (ebmm-associations 'ebmm-business-strategy nil))))
      "\n")
     "\"Business Strategy\" \"1..*\" --o \"1..*\" \"Business Initiative / Program\" : motivates
\"Business Strategy\" --> \"Business Model\" : motivates change towards
\"Business Strategy\" --> \"Business Strategy\" : enables
\"Business Strategy\" --> \"Business Strategy\" : influences
\"Business Strategy\" --> \"Influencer\" : responds to
\"Business Strategy\" --> \"Objective\" : achieves")
    ;; Business scorecard not in test view
    (should-error (ebmm-associations 'ebmm-business-scorecard nil))
    ;; Mission is in test view
    (should (ebmm-associations 'ebmm-mission nil))))

(provide 'enterprise-business-motivation-model-diagrams)
;;; enterprise-business-motivation-model-diagrams.el ends here
