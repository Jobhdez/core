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
                           :LEXP #S(PY-CONSTANT :NUM 30)
                           :REXP #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10))))
                #S(PY-PRINT :EXP #S(PY-VAR :NAME ZETTA-VAR::X))))
```

then the `remove-complex-operands` pass generates

```
(#S(ATOMIC-ASSIGNMENT
    :TEMP-VAR "temp_23"
    :N #S(PY-NEG-NUM :NUM #S(PY-CONSTANT :NUM 10)))
 #S(PY-ASSIGNMENT
    :NAME ZETTA-VAR::X
    :EXP #S(ATOMIC-SUM :LEXP #S(PY-CONSTANT :NUM 42) :REXP "temp_23"))
 #S(ATOMIC-ASSIGNMENT
    :TEMP-VAR "temp_25"
    :N #S(ATOMIC-SUM
          :LEXP ZETTA-VAR::X
          :REXP #S(PY-SUM
                   :LEXP #S(PY-VAR :NAME ZETTA-VAR::X)
                   :REXP #S(PY-CONSTANT :NUM 10))))
 #S(PY-PRINT :EXP "temp_25"))
 ```
 
 and then the `selection-instructions` pass returns:
 
 ```
 (#S(INSTRUCTION :NAME "movq" :ARG1 #S(IMMEDIATE :INT 42) :ARG2 ZETTA-VAR::X)
 #S(INSTRUCTION :NAME "addq" :ARG1 "temp_23" :ARG2 ZETTA-VAR::X)
 #S(INSTRUCTION :NAME "movq" :ARG1 ZETTA-VAR::X :ARG2 "temp_25")
 #S(INSTRUCTION :NAME "addq" :ARG1 #S(IMMEDIATE :INT 10) :ARG2 "temp_25")
 #S(INSTRUCTION :NAME "movq" :ARG1 "temp_25" :ARG2 "%rdi")
 #S(CALLQ :LABEL "print_int"))
 ```
 
 thanks
