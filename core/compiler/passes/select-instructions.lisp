(in-package #:core)

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

(defstruct free-pointer register)

(defstruct from-space register)

(defstruct tag t)

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
			 (cond ((and (py-var-p (py-sum-lexp e1))
				     (py-var-p (py-sum-rexp e1)))
				(let ((lvar (py-var-name (py-sum-lexp e1)))
				      (rvar (py-var-name (py-sum-rexp e1))))
				  (list (make-instruction :name "movq"
							  :arg1 (make-atomic-var :name rvar)
							  :arg2 "%rax")
					(make-instruction :name "addq"
							  :arg1 "%rax"
							  :arg2 (make-atomic-var :name lvar)))))
			       
			       ((and (py-constant-p (py-sum-rexp e1))
				     (py-var-p (py-sum-lexp e1)))
				(let ((lnum (py-constant-num (py-sum-rexp e1)))
				      (rvar (py-var-name (py-sum-lexp e1))))
				  (list (make-instruction :name "movq"
							  :arg1 (make-immediate :int lnum)
							  :arg2 "%rax")
					(make-instruction :name "addq"
							  :arg1 "%rax"
							  :arg2 (make-atomic-var :name rvar)))))))
			((py-sub-p e1)
			 (let ((var (py-sub-lexp e1)))
			   (if (py-var-p var)
			       (if (equalp (py-var-name var)
					   (atomic-var-name var-name))
				   (list (make-instruction :name "subq"
							   :arg1 "$1"
							   :arg2 var-name))))))

			((and (atomic-var-p var-name)
			      (atomic-var-p e1))
			 (make-instruction :name "movq"
					   :arg1 var-name
					   :arg2 e1))



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

			((atomic-sum-p n)
			 (list (make-instruction :name "movq"
						 :arg1 (atomic-sum-lexp n)
						 :arg2 tmp)
			       (make-instruction :name "addq"
						 :arg1 (atomic-sum-rexp n)
						 :arg2 tmp)))
			((py-cmp-p n)
			 (if (and (py-var-p (py-cmp-lexp n))
				  (py-constant-p (py-cmp-rexp n)))
			     (list (make-instruction :name "cmpq" :arg1 (make-immediate :int (py-constant-num (py-cmp-rexp n))) :arg2 (make-atomic-var :name (py-var-name (py-cmp-lexp n))))
				   (make-instruction :name "setl" :arg1 "%al" :arg2 'no-arg)
				   (make-instruction :name "movzbq" :arg1 "%al" :arg2 (make-atomic-var :name tmp)))))
			
			
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
		      (list (make-instruction :name "movq" :arg1 (py-var-name e1) :arg2 "%rdi")
			    (make-callq :label "print_int"))))


		 ((atomic-sum :lexp e1 :rexp e2)
		  (cond ((py-constant-p e1)
			 (list (make-instruction :name "addq"
						 :arg1 (py-constant-num e1)
						 :arg2 reg)
			       (make-instruction :name "retq"
						 :arg1 'no-arg
						 :arg2 'no-arg)))
			(t (error "E1 isnt a constant."))))
		 
		 ((if-goto-loop :cnd cnd :body loop-block :blocks blks)
		  (let ((blk1 (goto-block loop-block)))
		    (let ((body (gethash blk1 blks)))
		      (let ((cmp (py-cmp-cmp cnd)))
			(cond ((equalp "<" (string-upcase cmp))
			       (list (make-block-py :name blk1)
				     (mapcar (lambda (e) (select-instrs e)) body)
				     (make-instruction :name "cmpq" :arg1 (make-immediate :int (py-cmp-rexp cnd)) :arg2 (make-atomic-var :name (py-var-name (py-cmp-lexp cnd))))
				     (make-instruction :name "jl" :arg1 blk1 :arg2 'no-arg)))
			      (t
			       (list (mapcar (lambda (e) (select-instrs e)) body)
				     (make-instruction :name "cmpq" :arg1 (make-immediate :int (py-cmp-rexp cnd)) :arg2 (make-atomic-var :name (py-var-name (py-cmp-lexp cnd))))
				     (make-instruction :name "jg" :arg1 blk1 :arg2 'no-arg))))))))
		 
		 
		 ((if-goto :cnd cnd :thn thn :els els :blocks blks)
		  (let ((blk1 (goto-block thn))
			(blk2 (goto-block els)))
		    (let* ((exp-thn (gethash blk1 blks))
			   (exp-els (gethash blk2 blks)))
		      (list (make-instruction :name "cmpq" :arg1 (make-immediate :int 1) :arg2 cnd)
			    (make-instruction :name "je" :arg1 blk1 :arg2 'no-arg)
			    (make-instruction :name "jmp" :arg1 blk2 :arg2 'no-arg)
			    (make-block-py :name blk1)
			    (if (listp exp-thn) (mapcar (lambda (e) (select-instrs e)) exp-thn) (select-instrs exp-thn))
			    (make-instruction :name "jmp" :arg1 "conclusion" :arg2 'no-arg)
			    (make-block-py :name blk2)
			    (if (listp exp-els) (mapcar (lambda (e) (select-instrs e)) exp-els) (select-instrs exp-els))
			    (make-instruction :name "jmp" :arg1 "conclusion" :arg2 'no-arg)))))
		 ((while-atomic :loop-block loopb :test-block testb :pre-block preb)
		  (let ((setloopb (mapcar (lambda (n) (select-instrs n)) (if (listp loopb) loopb (list loopb))))
			(settestb (mapcar (lambda (n) (select-instrs n)) (if (listp testb) testb (list testb))))
			(setpreb (mapcar (lambda (n) (select-instrs n)) (if (listp preb) preb (list preb)))))
		    (list setpreb
			  (make-instruction :name "jmp" :arg1 "test" :arg2 'no-arg)
			  (make-instruction :name "jg" :arg1 "loop" :arg2 'no-arg)
			  (make-block-py :name "loop:")
			  setloopb
			  (make-block-py :name "test:")
			  settestb)))
		 
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
						     :arg2 (if (py-var-p e1) (make-atomic-var :name (py-var-name e1)) e1)))))
			((equalp "<" (string-upcase compare))
			 (list (make-instruction :name "cmpq"
						 :arg1 e2
						 :arg2 e1))))))))
      (alexandria::flatten (mapcar (lambda (n) (select-instrs n)) ast)))))



(defun list-of-atomic-assignment-p (statements)
  (cond ((null statements) T)
	((not (atomic-assignment-p (car statements))) nil)
	(t (and (atomic-assignment-p (car statements))
		(list-of-atomic-assignment-p (cdr statements))))))

(defun make-from-atomic-assignments (statements)
  (labels ((make-from (statement)
	     (match statement
	       ((atomic-assignment :temp-var var :n n)
		(cond ((py-neg-num-p n)
		       (list (make-instruction :name "movq" :arg1 (py-neg-num-num n) :arg2 var)
			     (make-instruction :name "subq" :arg1 var :arg2 'no-arg)))
		      ((atomic-sum-p n)
		       (list (make-instruction :name "movq"
					       :arg1 (atomic-sum-rexp n)
					       :arg1 "%rax")
			     (make-instruction :name "addq"
					       :arg1 (atomic-sum-lexp n)
					       :arg2 "%rax")))
		      (t (error "not valid instruction.")))))))
    (mapcar (lambda (statement) (make-from statement)) statements)))


(defvar namecounter 0)		     
(defun generate-fn-name (name)
  (progn
    (setf namecounter (+ namecounter 1))
    (concatenate 'string name (write-to-string namecounter))))



(defun select (exp)
  (let ((ast (parse-with-lexer (token-generator (lex-line exp)) *core-grammar*)))
    (let ((anf (remove-complex-operands ast)))
      (let ((cir (to-cir anf)))
	(select-instructions cir)))))

(defun ast->assembly (instructions)
  (labels ((toassembly (in)
             (match in
               ((instruction :name name :arg1 a1 :arg2 a2)
                (cond 
                  ((and (immediate-p a1) (stringp a2))
                   (concatenate 'string (string #\Tab) name " " "$"
				(write-to-string (immediate-int a1)) ", " 
				a2 (string #\Newline)))
		  
                  ((and (stringp a1) (stringp a2))
                   (concatenate 'string (string #\Tab) name " " 
				a1 ", " a2 (string #\Newline)))
                  
                  ((and (equalp name "jl") (stringp a1))
                   (concatenate 'string (string #\Tab) "jl " 
				a1 (string #\Newline)))
		  ((and (equalp name "je") (stringp a1))
                   (concatenate 'string (string #\Tab) "je " 
				a1 (string #\Newline)))
		  ((and (equalp name "jmp") (stringp a1))
                   (concatenate 'string (string #\Tab) "jmp " 
				a1 (string #\Newline)))
		  ((equalp name "setl")
		   (concatenate 'string (string #\Tab) name " " a1 (string #\Newline)))
                  
                  (t 
                   (error "Unsupported instruction format: ~A" in))))
	       ((block-py :name name)
		(concatenate 'string name ":" (string #\Newline)))
	       ((callq :label fn)
		(concatenate 'string (string #\Tab) "callq " fn (string #\Newline))))))
    
    (apply #'concatenate 'string (mapcar #'toassembly instructions))))
