(in-package :zetta-tests)

(deftest test-assignment ()
  (is (equalp (zetta:parse-with-lexer
	       (zetta:token-generator
		(zetta:lex-line "x = 50 + -10 print(x)"))
	       zetta::*python-grammar*)
	      (zetta:make-py-module :statements
				    (list (zetta:make-py-assignment
					   :name (quote zetta-var::x)
					   :exp (zetta:make-py-sum :lexp (zetta:make-py-constant :num 50)
								   :rexp (zetta:make-py-neg-num :num (zetta:make-py-constant :num 10))))
					  (zetta:make-py-print
					   :exp (zetta:make-py-var
						 :name (quote zetta-var::x))))))))
(deftest test-assignment-2 ()
  (is (equalp (zetta:parse-with-lexer
	       (zetta:token-generator
		(zetta:lex-line "x = 50 + -10 print(x + 10)"))
	       zetta::*python-grammar*)
	      (zetta:make-py-module :statements
				    (list (zetta:make-py-assignment
					   :name (quote zetta-var::x)
					   :exp (zetta:make-py-sum :lexp (zetta:make-py-constant :num 50)
								   :rexp (zetta:make-py-neg-num :num (zetta:make-py-constant :num 10))))
					  (zetta:make-py-print
					   :exp (zetta:make-py-sum :lexp (zetta:make-py-var :name (quote zetta-var::x))
								   :rexp (zetta:make-py-constant :num 10))))))))

(deftest test-assignment-3 ()
  (is (equalp (zetta:parse-with-lexer
	       (zetta:token-generator
		(zetta:lex-line "x = 10 + -3 y = 2 z = x + y print(z)"))
	       zetta::*python-grammar*)
	      (ZETTA:MAKE-PY-MODULE
	       :STATEMENTS
	       (list (ZETTA:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE ZETTA-VAR::X)
                   :EXP (ZETTA:MAKE-PY-SUM
                           :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 10)
                           :REXP (ZETTA:MAKE-PY-NEG-NUM :NUM (ZETTA:MAKE-PY-CONSTANT :NUM 3))))
                (ZETTA:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE ZETTA-VAR::Y)
                   :EXP (ZETTA:MAKE-PY-CONSTANT :NUM 2))
                (ZETTA:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE ZETTA-VAR::Z)
                   :EXP (ZETTA:MAKE-PY-SUM
                           :LEXP (MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::X))
                           :REXP (MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::Y))))
                     (MAKE-PY-PRINT :EXP (MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::Z))))))))

(deftest test-if ()
  (is (equalp (zetta:parse-with-lexer
	       (zetta:token-generator
		(zetta:lex-line "if 3==3: x = 10 + -3 print(x) else: y = 30 + -10 print(y)"))
	       zetta::*python-grammar*)
	      (ZETTA:MAKE-PY-MODULE
	       :STATEMENTS
	       (list
		 (ZETTA:MAKE-PY-IF
                   :EXP (ZETTA:MAKE-PY-CMP
                           :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 3)
                           :CMP (QUOTE :==)
                           :REXP (ZETTA:MAKE-PY-CONSTANT :NUM 3))
                   :IF-STATEMENT (list (ZETTA:MAKE-PY-ASSIGNMENT
                                     :NAME (quote ZETTA-VAR::X)
                                     :EXP (ZETTA:MAKE-PY-SUM
                                             :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 10)
                                             :REXP (ZETTA:MAKE-PY-NEG-NUM
                                                      :NUM (ZETTA:MAKE-PY-CONSTANT
                                                              :NUM 3))))
                                  (ZETTA:MAKE-PY-PRINT
                                     :EXP (ZETTA:MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::X))))
                   :ELSE-STATEMENT (list (MAKE-PY-ASSIGNMENT
                                       :NAME (QUOTE ZETTA-VAR::Y)
                                       :EXP (ZETTA:MAKE-PY-SUM
                                               :LEXP (ZETTA:MAKE-PY-CONSTANT :NUM 30)
                                               :REXP (ZETTA:MAKE-PY-NEG-NUM
                                                        :NUM (ZETTA:MAKE-PY-CONSTANT
                                                                :NUM 10))))
                                    (ZETTA:MAKE-PY-PRINT
                                       :EXP (ZETTA:MAKE-PY-VAR :NAME (QUOTE ZETTA-VAR::Y))))))))))
* 
