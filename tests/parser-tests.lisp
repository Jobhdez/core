(in-package :zetta-tests)

(deftest test-assignment ()
  (is (equalp (zetta:select-instructions
	       (zetta:remove-complex-operands
		(zetta:parse-with-lexer (zetta:token-generator (zetta:lex-line "x = 50 + -10 print(x)"))
				  zetta::*python-grammar*)))
	   (list (zetta:make-instruction
		  :NAME "movq"
		  :ARG1 10
		  :ARG2 (zetta:MAKE-ATOMIC-VAR
			 :NAME "temp_1"))
                 (zetta:make-instruction
		  :NAME "subq"
		  :ARG1 (zetta:MAKE-ATOMIC-VAR
			 :NAME "temp_1")
		  :ARG2 (quote NO-ARG))
                 (zetta:make-instruction
                  :NAME "movq"
                  :ARG1 (zetta:MAKE-IMMEDIATE
			 :INT 50)
                  :ARG2 (zetta:MAKE-ATOMIC-VAR
			 :NAME (quote ZETTA-VAR::X)))
                 (zetta:make-instruction
                  :NAME "addq"
                  :ARG1 (zetta:MAKE-ATOMIC-VAR
			 :NAME "temp_1")
                  :ARG2 (zetta:MAKE-ATOMIC-VAR
			 :NAME (quote ZETTA-VAR::X)))
                 (zetta:MAKE-CALLQ
		  :LABEL "print_int")))))


