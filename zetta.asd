(asdf:defsystem #:zetta
    :description "A compiler for a language with Python syntax."
    :author "Job Hernandez <hj93@protonmail.com>"
    :in-order-to ((asdf:test-op (asdf:test-op #:zetta/tests)))
    :depends-on (#:alexa #:yacc #:alexandria #:trivia #:hunchentoot #:com.inuoe.jzon)
    :serial t
    :pathname "zetta/"
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
		       :components ((:file "expose-allocation")
				    (:file "remove-complex-operands")
				    (:file "select-instructions")
				    (:file "assign-homes")))))))

(asdf:defsystem #:zetta/tests
  :description "Tests for zetta"
  :author "Job Hernandez"
  :depends-on (#:zetta #:fiasco)
  :perform (asdf:test-op (o s)
			 (unless (symbol-call :zetta-tests
					      :run-zetta-tests)
			   (error "Tests failed.")))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
	       (:file "parser-tests")
	       (:file "remove-complex-tests")
	       (:file "utilities")))
