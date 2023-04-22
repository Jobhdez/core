(in-package #:zetta)

;(defparameter *registers* (make-hash-table :test 'equal))
;(defparameter *expressions* (make-hash-table))

(defstruct immediate
  int)

(defstruct register
  reg)

(defstruct deref
  "Memory reference"
  reg
  int)

(defstruct instruction
  name
  arg1
  arg2)

(defstruct callq
  label)

(defstruct block-py name)

(defun select-instructions (ast)
  (let ((blocks (make-hash-table :test 'equalp)))
  (labels ((select-instrs (node)
	     (match node
	       ((py-assignment :name var-name
			       :exp e1)
		      (cond ((atomic-sum-p e1)
			     (let ((tmp-var (atomic-sum-rexp e1)))
				 (list (make-instruction :name "movq"
							 :arg1 (make-immediate :int (py-constant-num
									(atomic-sum-lexp e1)))
							 :arg2 var-name)
				       (make-instruction :name "addq"
							 :arg1 tmp-var
							 :arg2 var-name))))
			    ((py-constant-p  e1)
			     (make-instruction :name "movq"
					       :arg1 e1
					       :arg2 var-name))

			    ((py-sum-p e1)
			      (let ((tmp-var (py-sum-rexp e1)))
				 (list (make-instruction :name "movq"
							 :arg1 (py-sum-lexp e1)
							 :arg2 var-name)
				       (make-instruction :name "addq"
							 :arg1 (py-sum-rexp e1)
							 :arg2 var-name))))

			    ((py-sub-p e1)
			     (let ((var (py-sub-lexp e1)))
			       (if (py-var-p var)
				   (if (equalp (py-var-name var)
					       (atomic-var-name var-name))
				       (list (make-instruction :name "subq"
							       :arg1 "$1"
							       :arg2 var-name))))))


			    (t (error "Not valid PY-ASSIGNMENT."))))					 

		     ((atomic-assignment :temp-var tmp
					 :n n)
		      (cond ((atomic-sum-p n)
			  (let ((vari (atomic-sum-lexp n))
				(rexp (py-constant-num (atomic-sum-rexp n))))
			    (list (make-instruction :name "movq"
						    :arg1 rexp
						     :arg2 tmp)
				  (make-instruction :name "addq"
						    :arg1 vari
						    :arg2 tmp))))
			    ((py-neg-num-p n)

			     (let* ((num (py-constant-num (py-neg-num-num n)))
				    (tmp-var tmp))
			       ;(setf (gethash "%rax" *registers*) "%rax")
			       ;(setf (gethash 'py-neg-num *expressions*) 'py-neg-num)
			       (list
				(make-instruction :name "movq"
						  :arg1 num
						  :arg2 tmp-var)
				(make-instruction :name "subq"
						  :arg1 tmp-var
						  :arg2 'no-arg))))))
		     ((py-print :exp e1)
		      (if (or (py-var-p e1) (atomic-var-p e1))
			  (list (make-callq :label "print_int"))))


		     ((atomic-sum :lexp e1 :rexp e2)
		      (cond ((py-constant-p e1)
			     (list (make-instruction :name "addq"
						     :arg1 (py-constant-num e1)
						     :arg2 reg)
				    (make-instruction :name "retq"
						      :arg1 'no-arg
						      :arg2 'no-arg)))
			    (t (error "E1 isnt a constant."))))

		     ((if-atomic :block b1 :begin-then bthen :begin-else bels :condition e)
		      (let* ((set-instrs1 (mapcar (lambda (instr) (select-instrs instr))
						  bthen))
			     (set-instrs2 (mapcar (lambda (instr) (select-instrs instr))
						 bels))
			     (conditional (select-instrs e)))
			(list conditional
			      (make-instruction :name "je" :arg1 b1 :arg2 'no-arg)
			      (make-instruction :name "jmp" :arg1 "block_2" :arg2 'no-arg)
			      (make-block-py :name b1)
			      set-instrs1
			      (make-block-py :name "block_2")
			      set-instrs2)))

		     ((while-atomic :loop-block loopb :test-block testb :pre-block preb)
		      (let ((setloopb (mapcar (lambda (n) (select-instrs n)) (if (listp loopb) loopb (list loopb))))
			    (settestb (mapcar (lambda (n) (select-instrs n)) (if (listp testb) testb (list testb))))
			    (setpreb (mapcar (lambda (n) (select-instrs n)) (if (listp preb) preb (list preb)))))
			(list setpreb
			      (make-instruction :name "jmp" :arg1 "test" :arg2 'no-arg)
			      "loop:"
			      setloopb
			      "test:"
			      settestb
			      (make-instruction :name "jg" :arg1 "loop" :arg2 'no-arg))))

		     ((py-cmp :lexp e1 :cmp compare :rexp e2)
		      (cond ((equalp "==" (string-upcase compare))
			  (list (make-instruction :name "movq"
						  :arg1 (make-immediate :int (py-constant-num e1))
						  :arg2 "%rsi")
				(make-instruction :name "movq"
						  :arg1 (make-immediate :int (py-constant-num e2))
						  :arg2 "%rdi")
				(make-instruction :name "cmpq"
						  :arg1 "%rsi"
						  :arg2 "%rdi")))
			    ((equalp ">" (string-upcase compare))
			     (if (equalp 1 (py-constant-num e2))
				 (list (make-instruction :name "cmpq"
						     :arg1 "$1"
							 :arg2 (if (py-var-p e1) (make-atomic-var :name (py-var-name e1)) e1))))))))))
	    (alexandria::flatten (mapcar (lambda (n) (select-instrs n)) ast)))))




