(define-module (ebmm-package)
  #:use-modules (guix)
  #:use-modules (gnu build-system emacs)
  #:use-modules (gnu packages emacs-xyz)
  #:use-modules (gnu packages uml)
  #:use-modules (guix git-download))

(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public emacs-ebmm
  (package
    (name "emacs-ebmm")
    (version "0.5.0-git")
    (build-system emacs-build-system)
    (propagated-inputs
     (list emacs-plantuml-mode
	   plantuml))
    (source (local-file "../.." "eamcs-ebmm-checkout"
			#:recursive? #t
			#:select? vcs-file?))))

emacs-ebmm
