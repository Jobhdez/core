(asdf:defsystem #:core
  :description "A compiler for the core of imperative features"
  :author "Job Hernandez <hj93@protonmail.com>"
  :in-order-to ((asdf:test-op (asdf:test-op #:core/tests)))
  :depends-on (#:alexa #:yacc #:alexandria #:trivia #:hunchentoot #:com.inuoe.jzon)
  :serial t
  :pathname "core/"
  :components
  ((:file "package")
   (:module "compiler"
    :components
    ((:module "lexer"
      :components ((:file "lexer")))
     (:module "parser"
      :components ((:file "parser")
		   (:file "ast")))
     (:module "passes"
      :components ((:file "remove-complex-operands")
		   (:file "to-cir")
		   (:file "select-instructions")
		   (:file "assign-homes")))))))

(asdf:defsystem #:core/tests
  :description "Tests for zetta"
  :author "Job Hernandez"
  :depends-on (#:zetta #:fiasco)
  :perform (asdf:test-op (o s)
			 (unless (symbol-call :core-tests
					      :run-core-tests)
			   (error "Tests failed.")))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
	       (:file "parser-tests")
	       (:file "remove-complex-tests")
	       (:file "utilities")))
