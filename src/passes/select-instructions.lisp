(in-package #:zetta)

(defparameter *registers* (make-hash-table :test 'equal))
(defparameter *expressions* (make-hash-table))

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

(defun select-instructions (ast)
  (flatten (mapcar (lambda (n) (select-instrs n)) ast)))



(defun select-instrs (node)
  (match node
	 ((py-assignment :name n
			 :exp e1)
	  (if (atomic-sum-p e1)
	      (let* ((tmp-var (atomic-sum-rexp e1))
		    (hash-values (hash-table-values *expressions*))
		    (hash-keys (hash-table-keys *expressions*))
		    (last-key (car (last hash-keys))))
		     
		(list (make-instruction :name "movq"
				        :arg1 (make-immediate :int (py-constant-num
							    (atomic-sum-lexp e1)))
				        :arg2 n)
		      (if (equalp 'py-neg-num last-key)
			  (make-instruction :name "addq"
				        :arg1 n
				        :arg2 (gethash "%rax" *registers*))
			  (make-instruction :name "addq"
					    :arg1 tmp-var
					    :arg2 n))))))
				
						 
						 
		        
	 ((atomic-assignment :temp-var tmp
			     :n n)
	  (cond ((atomic-sum-p n)
	      (let ((vari (atomic-sum-lexp n))
		    (rexp (py-constant-num (py-sum-rexp (atomic-sum-rexp n)))))
		(list (make-instruction :name "movq"
					:arg1 (if (equalp 'py-neg-num (gethash 'py-neg-num *expressions*))
						  "%rax"
						  vari)
				 :arg2 tmp)
		      (make-instruction :name "addq"
					:arg1 (make-immediate :int rexp)
					:arg2 tmp))))
		((py-neg-num-p n)
		 (let* ((num (py-constant-num (py-neg-num-num n)))
			(tmp-var tmp))
		   (setf (gethash "%rax" *registers*) "%rax")
		   (setf (gethash 'py-neg-num *expressions*) 'py-neg-num)
		   (list
		    (make-instruction :name "movq"
				      :arg1 num
				      :arg2 tmp-var)
		    (make-instruction :name "negq"
				      :arg1 tmp-var
				      :arg2 'no-arg)
		    (make-instruction :name "movq"
				      :arg1 tmp-var
				      :arg2 (gethash "%rax" *registers*)))))))
	 ((py-print :exp e1)
	  (let* ((hash-values (hash-table-values *expressions*))
	      	 (hash-keys (hash-table-keys *expressions*))
		 (last-key (car (last hash-keys))))
	    (list (if (equalp 'py-neg-num last-key)
		      (make-instruction :name "movq"
				  :arg1 "%rax"
				  :arg2 "%rdi")
		      (make-instruction :name "movq"
					:arg1 e1
					:arg2 "%rdi"))
		
		(make-callq :label "print_int"))))
	 ((atomic-sum :lexp e1 :rexp e2)
	  (cond ((py-constant-p e1)
		 
		 (let* ((hash-values (hash-table-values *registers*))
		       (hash-keys (hash-table-values *registers*))
		       (last-key (car (last hash-keys)))
		       (reg (gethash last-key *registers*)))
		   (remhash last-key *registers*)
		   (list (make-instruction :name "addq"
					   :arg1 (py-constant-num e1)
					   :arg2 reg)
			 (make-instruction :name "retq"
					   :arg1 'no-arg
					   :arg2 'no-arg))))
		(t (error "E1 isnt a constant."))))))
		
		   
				  
			   

	  
	 
