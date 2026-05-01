(define-module (ebmm-package)
  #:use-module (guix)
  #:use-module (guix build-system emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages uml)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public emacs-ebmm
  (package
    (name "emacs-ebmm")
    (version "0.1.0-git")
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-plantuml-mode
	   plantuml))
    (source (local-file "../.." "emacs-ebmm-checkout"
			#:recursive? #t
			#:select? vcs-file?))
    (arguments
     (list
      #:test-command #~(list "emacs" "-Q" "--batch"
			     "-L" "."
			     "-L" "tests/"
			     "-l" "enterprise-business-motivation-model-diagrams.el"
			     "-f" "ert-run-tests-batch-and-exit")
      #:exclude #~(cons "^tests/" %default-exclude)))
    (home-page #f)
    (synopsis "Enterprise Business Motivation Model in Emacs")
    (description "Browse elements and views from the Enterprise Business Motivation
Model in Emacs.")
    (license license:cc0)))

emacs-ebmm
