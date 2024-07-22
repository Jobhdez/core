(in-package :core-tests)

(deftest test-assignment-ast ()
  (let ((node (core:parse-with-lexer (core:token-generator (core:lex-line "x = 50 + -10 print(x)"))
				      core::*python-grammar*)))
	(is (equalp (remove-complex-operands node)
		    (list (CORE:MAKE-ATOMIC-ASSIGNMENT
			:TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")
			:N (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 10)))
		         (CORE:MAKE-PY-ASSIGNMENT
			  :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X))
			  :EXP (CORE:MAKE-ATOMIC-SUM
				  :LEXP (CORE:MAKE-PY-CONSTANT :NUM 50)
				  :REXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")))
			 (CORE:MAKE-PY-PRINT :EXP (CORE:MAKE-PY-VAR :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X)))))))))

(deftest test-assignment-ast2 ()
  (let ((node (core:parse-with-lexer (core:token-generator (core:lex-line "x = 50 + -10 print(x + 10)"))
				      core::*python-grammar*)))
	(is (equalp (remove-complex-operands node)
		    (list (CORE:MAKE-ATOMIC-ASSIGNMENT
			:TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")
			:N (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 10)))
		        (CORE:MAKE-PY-ASSIGNMENT
			  :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X))
			  :EXP (CORE:MAKE-ATOMIC-SUM
				  :LEXP (CORE:MAKE-PY-CONSTANT :NUM 50)
				  :REXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")))
		       (CORE:MAKE-ATOMIC-ASSIGNMENT
			  :TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_3")
			  :N (CORE:MAKE-ATOMIC-SUM
				:LEXP (CORE:MAKE-ATOMIC-VAR :NAME (CORE:MAKE-PY-VAR :NAME (QUOTE CORE-VAR::X)))
				:REXP (CORE:MAKE-PY-CONSTANT :NUM 10)))
		       (CORE:MAKE-PY-PRINT :EXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_3")))))))



(deftest test-assignment-ast3 ()
  (let ((node (core:parse-with-lexer (core:token-generator (core:lex-line "x = 10 + -3 y = 2 z = x + y print(z)"))
				      core::*python-grammar*)))
    (is (equalp (remove-complex-operands node)
		(list (CORE:MAKE-ATOMIC-ASSIGNMENT
		    :TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")
		    :N (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 3)))
		      
		   (CORE:MAKE-PY-ASSIGNMENT
		      :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X))
		      :EXP (CORE:MAKE-ATOMIC-SUM
			      :LEXP (CORE:MAKE-PY-CONSTANT :NUM 10)
			      :REXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")))
		   
		   (CORE:MAKE-PY-ASSIGNMENT
		      :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::Y))
		      :EXP (CORE:MAKE-PY-CONSTANT :NUM 2))
		   
		   (CORE:MAKE-PY-ASSIGNMENT
		      :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::Z))
		      :EXP (CORE:MAKE-PY-SUM
			      :LEXP (CORE:MAKE-PY-VAR :NAME (QUOTE CORE-VAR::X))
			      :REXP (CORE:MAKE-PY-VAR :NAME (QUOTE CORE-VAR::Y))))
		   
		   (CORE:MAKE-PY-PRINT :EXP (CORE:MAKE-PY-VAR :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::Z)))))))))


(deftest test-if-ast ()
  (let ((node (core:parse-with-lexer (core:token-generator (core:lex-line "if 3==3: x = 10 + -3 print(x) else: y = 30 + -10 print(y)"))
				      core::*python-grammar*)))
    (is (equalp (remove-complex-operands node)
		(list (CORE:MAKE-IF-ATOMIC
		    :BLOCK "block_1"
		    :BEGIN-THEN (list (CORE:MAKE-ATOMIC-ASSIGNMENT
				    :TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")
				    :N (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 3)))
				      
				   (CORE:MAKE-PY-ASSIGNMENT
				      :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X))
				      :EXP (CORE:MAKE-ATOMIC-SUM
					      :LEXP (CORE:MAKE-PY-CONSTANT :NUM 10)
					      :REXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_1")))
				   
				   (CORE:MAKE-PY-PRINT
				      :EXP (CORE:MAKE-PY-VAR :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::X)))))
		    :BEGIN-ELSE (list (CORE:MAKE-ATOMIC-ASSIGNMENT
				    :TEMP-VAR (CORE:MAKE-ATOMIC-VAR :NAME "temp_3")
				    :N (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 10)))
				      
				   (CORE:MAKE-PY-ASSIGNMENT
				      :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::Y))
				      :EXP (CORE:MAKE-ATOMIC-SUM
					      :LEXP (CORE:MAKE-PY-CONSTANT :NUM 30)
                                              :REXP (CORE:MAKE-ATOMIC-VAR :NAME "temp_3")))
				      
				   (CORE:MAKE-PY-PRINT
                                      :EXP (CORE:MAKE-PY-VAR :NAME (CORE:MAKE-ATOMIC-VAR :NAME (QUOTE CORE-VAR::Y)))))
		    :CONDITION (CORE:MAKE-PY-CMP
				  :LEXP (CORE:MAKE-PY-CONSTANT :NUM 3)
                                  :CMP (QUOTE :==)
                                  :REXP (CORE:MAKE-PY-CONSTANT
					   :NUM 3))))))))
    
