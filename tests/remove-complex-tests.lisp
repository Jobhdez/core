(in-package :zetta-tests)

(deftest test-assignment-ast ()
  (let ((node (zetta:parse-with-lexer (zetta:token-generator (zetta:lex-line "x = 50 + -10 print(x)"))
				      zetta::*python-grammar*)))
	(is (equalp (remove-complex-operands node)
		    (list (ZETTA:MAKE-ATOMIC-ASSIGNMENT
			:TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")
			:N (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 10)))
		         (ZETTA:MAKE-PY-ASSIGNMENT
			  :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X))
			  :EXP (ZETTA:MAKE-ATOMIC-SUM
				  :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 50)
				  :REXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")))
			 (ZETTA:MAKE-PY-PRINT :EXP (ZETTA:MAKE-PY-VAR :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X)))))))))

(deftest test-assignment-ast2 ()
  (let ((node (zetta:parse-with-lexer (zetta:token-generator (zetta:lex-line "x = 50 + -10 print(x + 10)"))
				      zetta::*python-grammar*)))
	(is (equalp (remove-complex-operands node)
		    (list (ZETTA:MAKE-ATOMIC-ASSIGNMENT
			:TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")
			:N (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 10)))
		        (ZETTA:MAKE-PY-ASSIGNMENT
			  :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X))
			  :EXP (ZETTA:MAKE-ATOMIC-SUM
				  :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 50)
				  :REXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")))
		       (ZETTA:MAKE-ATOMIC-ASSIGNMENT
			  :TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_3")
			  :N (ZETTA:MAKE-ATOMIC-SUM
				:LEXP (ZETTA:MAKE-ATOMIC-VAR :NAME (ZETTA:MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::X)))
				:REXP (ZETTA:MAKE-PY-CONSTANT :NUM 10)))
		       (ZETTA:MAKE-PY-PRINT :EXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_3")))))))



(deftest test-assignment-ast3 ()
  (let ((node (zetta:parse-with-lexer (zetta:token-generator (zetta:lex-line "x = 10 + -3 y = 2 z = x + y print(z)"))
				      zetta::*python-grammar*)))
    (is (equalp (remove-complex-operands node)
		(list (ZETTA:MAKE-ATOMIC-ASSIGNMENT
		    :TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")
		    :N (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 3)))
		      
		   (ZETTA:MAKE-PY-ASSIGNMENT
		      :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X))
		      :EXP (ZETTA:MAKE-ATOMIC-SUM
			      :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 10)
			      :REXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")))
		   
		   (ZETTA:MAKE-PY-ASSIGNMENT
		      :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::Y))
		      :EXP (ZETTA:MAKE-PY-CONSTANT :NUM 2))
		   
		   (ZETTA:MAKE-PY-ASSIGNMENT
		      :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::Z))
		      :EXP (ZETTA:MAKE-PY-SUM
			      :LEXP (ZETTA:MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::X))
			      :REXP (ZETTA:MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::Y))))
		   
		   (ZETTA:MAKE-PY-PRINT :EXP (ZETTA:MAKE-PY-VAR :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::Z)))))))))


(deftest test-if-ast ()
  (let ((node (zetta:parse-with-lexer (zetta:token-generator (zetta:lex-line "if 3==3: x = 10 + -3 print(x) else: y = 30 + -10 print(y)"))
				      zetta::*python-grammar*)))
    (is (equalp (remove-complex-operands node)
		(list (ZETTA:MAKE-IF-ATOMIC
		    :BLOCK "block_1"
		    :BEGIN-THEN (list (ZETTA:MAKE-ATOMIC-ASSIGNMENT
				    :TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")
				    :N (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 3)))
				      
				   (ZETTA:MAKE-PY-ASSIGNMENT
				      :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X))
				      :EXP (ZETTA:MAKE-ATOMIC-SUM
					      :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 10)
					      :REXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_1")))
				   
				   (ZETTA:MAKE-PY-PRINT
				      :EXP (ZETTA:MAKE-PY-VAR :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::X)))))
		    :BEGIN-ELSE (list (ZETTA:MAKE-ATOMIC-ASSIGNMENT
				    :TEMP-VAR (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_3")
				    :N (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 10)))
				      
				   (ZETTA:MAKE-PY-ASSIGNMENT
				      :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::Y))
				      :EXP (ZETTA:MAKE-ATOMIC-SUM
					      :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 30)
                                              :REXP (ZETTA:MAKE-ATOMIC-VAR :NAME "temp_3")))
				      
				   (ZETTA:MAKE-PY-PRINT
                                      :EXP (ZETTA:MAKE-PY-VAR :NAME (ZETTA:MAKE-ATOMIC-VAR :NAME (QUOTE ZETTA-VAR::Y)))))
		    :CONDITION (ZETTA:MAKE-PY-CMP
				  :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 3)
                                  :CMP (QUOTE :==)
                                  :REXP (ZETTA:MAKE-PY-CONSTANT
					   :NUM 3))))))))
    
