# zettapy
python compiler

Compiler for a subset of python3 or at least a compiler for a python like language :-)

Work in progress


## Example

given the expression `"x = 42 + -10 print(x + 10)"` the parser generates:

```
#S(PY-MODULE
   :STATEMENTS (#S(PY-ASSIGNMENT
                   :NAME ZETTA-VAR::X
                   :EXP #S(PY-SUM
                           :LEXP #S(PY-CONSTANT :NUM 42)
                           :REXP #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10))))
                #S(PY-PRINT
                   :EXP #S(PY-SUM
                           :LEXP #S(PY-VAR :NAME ZETTA-VAR::X)
                           :REXP #S(PY-CONSTANT :NUM 10)))))

```

then the `remove-complex-operands` pass generates

```

(#S(ATOMIC-ASSIGNMENT
    :TEMP-VAR "temp_6"
    :N #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10)))
 #S(PY-ASSIGNMENT
    :NAME ZETTA-VAR::X
    :EXP #S(ATOMIC-SUM :LEXP #S(PY-CONSTANT :NUM 42) :REXP "temp_6"))
 #S(ATOMIC-ASSIGNMENT
    :TEMP-VAR "temp_8"
    :N #S(ATOMIC-SUM
          :LEXP ZETTA-VAR::X
          :REXP #S(PY-SUM
                   :LEXP #S(PY-VAR :NAME ZETTA-VAR::X)
                   :REXP #S(PY-CONSTANT :NUM 10))))

```
 
 and then the `selection-instructions` pass returns:
 
 ```
 (#S(INSTRUCTION :NAME "movq" :ARG1 10 :ARG2 "temp_6")
 #S(INSTRUCTION :NAME "negq" :ARG1 "temp_6" :ARG2 NO-SEC-ARG)
 #S(INSTRUCTION :NAME "movq" :ARG1 "temp_6" :ARG2 "%rax")
 #S(INSTRUCTION :NAME "movq" :ARG1 #S(IMMEDIATE :INT 42) :ARG2 ZETTA-VAR::X)
 #S(INSTRUCTION :NAME "addq" :ARG1 ZETTA-VAR::X :ARG2 "%rax")
 #S(INSTRUCTION :NAME "movq" :ARG1 "%rax" :ARG2 "temp_8")
 #S(INSTRUCTION :NAME "addq" :ARG1 #S(IMMEDIATE :INT 10) :ARG2 "temp_8")
 #S(INSTRUCTION :NAME "movq" :ARG1 "%rax" :ARG2 "%rdi")
 #S(CALLQ :LABEL "print_int"))
 ```
 
 # Example 2
 
 `52 + -10` =>
 
 ```
 #S(PY-MODULE
   :STATEMENTS (#S(PY-SUM
                   :LEXP #S(PY-CONSTANT :NUM 52)
                   :REXP #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10)))))
 ```
 
 `remove-complex-operands` ->
 
 ```
 (#S(ATOMIC-ASSIGNMENT
    :TEMP-VAR "temp_9"
    :N #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10)))
 #S(ATOMIC-SUM :LEXP #S(PY-CONSTANT :NUM 52) :REXP "temp_9"))
 
 ```
 
 `selection-instructions` ->
 
 ```
 (#S(INSTRUCTION :NAME "movq" :ARG1 10 :ARG2 "temp_9")
 #S(INSTRUCTION :NAME "negq" :ARG1 "temp_9" :ARG2 NO-SEC-ARG)
 #S(INSTRUCTION :NAME "movq" :ARG1 "temp_9" :ARG2 "%rax")
 #S(INSTRUCTION :NAME "addq" :ARG1 52 :ARG2 "%rax")
 #S(INSTRUCTION :NAME "retq" :ARG1 NO-ARG :ARG2 NO-ARG))
 ```
 thanks
