(in-package :core-tests)

(deftest test-assignment ()
  (is (equalp (core:parse-with-lexer
	       (core:token-generator
		(core:lex-line "x = 50 + -10 print(x)"))
	       core::*python-grammar*)
	      (core:make-py-module :statements
				    (list (core:make-py-assignment
					   :name (quote core-var::x)
					   :exp (core:make-py-sum :lexp (core:make-py-constant :num 50)
								   :rexp (core:make-py-neg-num :num (core:make-py-constant :num 10))))
					  (core:make-py-print
					   :exp (core:make-py-var
						 :name (quote core-var::x))))))))
(deftest test-assignment-2 ()
  (is (equalp (core:parse-with-lexer
	       (core:token-generator
		(core:lex-line "x = 50 + -10 print(x + 10)"))
	       core::*python-grammar*)
	      (core:make-py-module :statements
				    (list (core:make-py-assignment
					   :name (quote core-var::x)
					   :exp (core:make-py-sum :lexp (core:make-py-constant :num 50)
								   :rexp (core:make-py-neg-num :num (core:make-py-constant :num 10))))
					  (core:make-py-print
					   :exp (core:make-py-sum :lexp (core:make-py-var :name (quote core-var::x))
								   :rexp (core:make-py-constant :num 10))))))))

(deftest test-assignment-3 ()
  (is (equalp (core:parse-with-lexer
	       (core:token-generator
		(core:lex-line "x = 10 + -3 y = 2 z = x + y print(z)"))
	       core::*python-grammar*)
	      (CORE:MAKE-PY-MODULE
	       :STATEMENTS
	       (list (CORE:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE CORE-VAR::X)
                   :EXP (CORE:MAKE-PY-SUM
                           :LEXP (CORE:MAKE-PY-CONSTANT :NUM 10)
                           :REXP (CORE:MAKE-PY-NEG-NUM :NUM (CORE:MAKE-PY-CONSTANT :NUM 3))))
                (CORE:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE CORE-VAR::Y)
                   :EXP (CORE:MAKE-PY-CONSTANT :NUM 2))
                (CORE:MAKE-PY-ASSIGNMENT
                   :NAME (QUOTE CORE-VAR::Z)
                   :EXP (CORE:MAKE-PY-SUM
                           :LEXP (MAKE-PY-VAR :NAME (QUOTE CORE-VAR::X))
                           :REXP (MAKE-PY-VAR :NAME (QUOTE CORE-VAR::Y))))
                     (MAKE-PY-PRINT :EXP (MAKE-PY-VAR :NAME (QUOTE CORE-VAR::Z))))))))

(deftest test-if ()
  (is (equalp (core:parse-with-lexer
	       (core:token-generator
		(core:lex-line "if 3==3: x = 10 + -3 print(x) else: y = 30 + -10 print(y)"))
	       core::*python-grammar*)
	      (CORE:MAKE-PY-MODULE
	       :STATEMENTS
	       (list
		 (CORE:MAKE-PY-IF
                   :EXP (CORE:MAKE-PY-CMP
                           :LEXP (CORE:MAKE-PY-CONSTANT :NUM 3)
                           :CMP (QUOTE :==)
                           :REXP (CORE:MAKE-PY-CONSTANT :NUM 3))
                   :IF-STATEMENT (list (CORE:MAKE-PY-ASSIGNMENT
                                     :NAME (quote CORE-VAR::X)
                                     :EXP (CORE:MAKE-PY-SUM
                                             :LEXP (CORE:MAKE-PY-CONSTANT :NUM 10)
                                             :REXP (CORE:MAKE-PY-NEG-NUM
                                                      :NUM (CORE:MAKE-PY-CONSTANT
                                                              :NUM 3))))
                                  (CORE:MAKE-PY-PRINT
                                     :EXP (CORE:MAKE-PY-VAR :NAME (QUOTE CORE-VAR::X))))
                   :ELSE-STATEMENT (list (MAKE-PY-ASSIGNMENT
                                       :NAME (QUOTE CORE-VAR::Y)
                                       :EXP (CORE:MAKE-PY-SUM
                                               :LEXP (CORE:MAKE-PY-CONSTANT :NUM 30)
                                               :REXP (CORE:MAKE-PY-NEG-NUM
                                                        :NUM (CORE:MAKE-PY-CONSTANT
                                                                :NUM 10))))
                                    (CORE:MAKE-PY-PRINT
                                       :EXP (CORE:MAKE-PY-VAR :NAME (QUOTE CORE-VAR::Y))))))))))
* 
