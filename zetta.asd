(asdf:defsystem #:zetta
    :description "A Python3 compiler."
    :author "Job Hernandez <hj93@protonmail.com>"
    :in-order-to ((asdf:test-op (asdf:test-op #:yotta/tests)))
    :depends-on (#:alexa #:yacc #:alexandria #:trivia)
    :serial t
    :pathname "src/"
    :components
    ((:file "package")
     (:module "lexer"
	      :components ((:file "lexer")))
     (:module "parser"
	      :components ((:file "parser")
			   (:file "ast")))
     (:module "passes"
	      :components ((:file "remove-complex-operands")))))
