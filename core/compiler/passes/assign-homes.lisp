(in-package #:core)

(defun patch-instructions (instr)
  (match instr
    ((instruction :name name :arg1 a1 :arg2 a2)
     (if (and (equalp name "addq")
	      (atomic-var-p a1)
	      (atomic-var-p a2))
	 (list (make-instruction :name name
				 :arg1 a1
				 :arg2 "%rax")
	       (make-instruction :name name
				 :arg1 "%rax"
				 :arg2 a2))
	 (make-instruction :name name
			   :arg1 a1
			   :arg2 a2)))
    ((callq :label la)
     (make-callq :label la))))

(defun assign-homes (instructions)
  (let ((*varnames* (make-hash-table :test 'equalp))
	(*variable* 0))
    (labels ((assignh (instr)
	       (match instr
	         ((instruction :name name :arg1 a1 :arg2 a2)
	          (cond ((and (numberp a1)
		              (atomic-var-p a2))
		         (if (gethash a2 *varnames*)
		             (make-instruction :name name
				               :arg1 a1
				               :arg2 (gethash a2 *varnames*))
		             (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a2 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
			            (make-instruction :name name
					              :arg1 a1
				                      :arg2 (gethash a2 *varnames*)))))
			((and (immediate-p a1) (stringp a2))
			 instr)

			;; registers
			((and (stringp a1) (stringp a2))
			 instr)

			((and (stringp a1) (symbolp a2))
			 instr)

		        ((and (immediate-p a1)
		              (atomic-var-p a2))
		         (if (gethash a2 *varnames*)
		             (make-instruction :name name
				               :arg1 a1
				               :arg2 (gethash a2 *varnames*))
		             (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a2 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
			            (make-instruction :name name
					              :arg1 a1
				                      :arg2 (gethash a2 *varnames*)))))
			((and (atomic-var-p a1)
			      (stringp a2))
			 (if (gethash a1 *varnames*)
			     (make-instruction :name name
				               :arg1 (gethash a1 *varnames*)
				               :arg2 a2)
			     (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a1 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
			            (make-instruction :name name
				                      :arg1 (gethash a1 *varnames*)
				                      :arg2 a2))))
			((and (stringp a1)
		              (atomic-var-p a2))
			 (if (gethash a2 *varnames*)
			     (make-instruction :name name
				               :arg1 a1
				               :arg2 (gethash a2 *varnames*))
			     (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a2 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
			            (make-instruction :name name
				                      :arg1 a1
				                      :arg2 (gethash a2 *varnames*)))))
			((and (atomic-var-p a1)
			      (atomic-var-p a2))
			 (cond ((and (gethash a1 *varnames*)
				     (gethash a2 *varnames*))
				(make-instruction :name name
						  :arg1 (gethash a1 *varnames*)
						  :arg2 (gethash a2 *varnames*)))
			       ((and (gethash a1 *varnames*)
				     (not (gethash a2 *varnames*)))
				(progn (setf *variable* (+ *variable* 8))
				       (setf (gethash a2 *varnames*)
					     (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
				       (make-instruction :name name
							 :arg1 (gethash a1 *varnames*)
							 :arg2 (gethash a2 *varnames*))))
			       ((and (not (gethash a1 *varnames*))
				     (gethash a2 *varnames*))
				(progn (setf *variable* (+ *variable* 8))
				       (setf (gethash a2 *varnames*)
					     (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
				       (make-instruction :name name
							 :arg1 (gethash a1 *varnames*)
							 :arg2 (gethash a2 *varnames*))))))
			
			
			((and (atomic-var-p a1)
		              (symbolp a2))
			 (if (gethash a1 *varnames*)
			     (make-instruction :name name
				               :arg1 (gethash a1 *varnames*)
				               :arg2 a2)
			     (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a1 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
				    (make-instruction :name name
				                      :arg1 (gethash a1 *varnames*)
				                      :arg2 a2))))
			((and (py-constant-p a1)
		              (atomic-var-p a2))
			 (if (gethash a2 *varnames*)
			     (make-instruction :name name
				               :arg1 a1
				               :arg2 (gethash a2 *varnames*))
			     (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a2 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
				    (make-instruction :name name
					              :arg1 a1
					              :arg2 (gethash a2 *varnames*)))))
			((and (py-var-p a1)
		              (atomic-var-p a2))
			 (if (gethash a2 *varnames*)
			     (make-instruction :name name
				               :arg1 a1
				               :arg2 (gethash a2 *varnames*))
			     (progn (setf *variable* (+ *variable* 8))
		                    (setf (gethash a2 *varnames*)
		                          (concatenate 'string "-" (write-to-string *variable*) "(%rbp)"))
				    (make-instruction :name name
					              :arg1 a1
					              :arg2 (gethash a2 *varnames*)))))
			(t (error "Not valid Instruction."))))
		 ((block-py :name name)
		  instr)
		 ((callq :label lbl)
		  instr))))
      (let ((instrs (flatten (mapcar (lambda (instr) (assignh  instr))  instructions))))
	(list *variable* instrs)))))



(defun clean (ast)
  (labels ((clean-select-pass (instr)
	     (match instr
	       ((instruction :name name :arg1 a1 :arg2 a2)
		(cond ((and (atomic-var-p a1) (py-var-p (atomic-var-name a1)))
		       (make-instruction :name name
					 :arg1 (make-atomic-var :name (py-var-name (atomic-var-name a1)))
					 :arg2 a2))
		      ((and (py-var-p a1) (not (py-var-p a2)))
		       (make-instruction :name name
					 :arg1 (make-atomic-var :name (py-var-name a1))
					 :arg2 a2))
		      ((and (py-constant-p a1) (py-var-p a2))
		       (make-instruction :name name
					 :arg1 (make-immediate :int (py-constant-num a1))
					 :arg2 (make-atomic-var :name (py-var-name a2))))
		      ((py-constant-p a1)
		       (make-instruction :name name
					 :arg1 (make-immediate :int (py-constant-num a1))
					 :arg2 a2))
		      ((and (stringp a1) (py-var-p a2))
		       (make-instruction :name name
					 :arg1 a1
					 :arg2 (make-atomic-var :name (py-var-name a2))))
		      ((and (py-var-p a1) (py-var-p a2))
		       (make-instruction :name name
					 :arg1 (make-atomic-var :name (py-var-name a1))
					 :arg2 (make-atomic-var :name (py-var-name a2))))

		      ((and (immediate-p a1) (py-var-p a2))
		       (make-instruction :name name
					 :arg1 a1
					 :arg2 (make-atomic-var :name (py-var-name a2))))

		      ((immediate-p a1)
		       (if (py-constant-p (immediate-int a1))
			   (make-instruction :name name
					     :arg1 (make-immediate :int (py-constant-num (immediate-int a1)))
					     :arg2 a2)
			   instr))
		      
		      (t
		       instr)))
	       ((block-py :name name)
		instr)
	       
	       ((callq :label label)
		instr))))
    (flatten (mapcar (lambda (instr) (clean-select-pass instr)) ast))))

(defun without-last (lst)
  (reverse (cdr (reverse lst))))

(defun ast->x86 (ins)
  (let ((cins (clean ins)))
    (let ((ins* (assign-homes cins)))
      (let ((stack (car ins*)))
	(let ((instructions (car (cdr ins*))))
	  (let ((stack* (if (equalp (mod stack 16) 0) stack (+ stack 8))))
	    (let ((assembly (ast->assembly instructions)))
	      (let ((prelude (concatenate 'string
					  (string #\Tab)
					  ".globl main"
					  (string #\Newline)
					  "main:"
					  (string #\Newline)
					  (string #\Tab)
					  "pushq %rbp"
					  (string #\Newline)
					  (string #\Tab)
					  "movq %rsp, %rbp"
					  (string #\Newline)
					  (string #\Tab)
					  "subq $"
					  (write-to-string stack*)
					  ", "
					  "%rsp"
					  (string #\Newline))))
		(let ((conclusion (concatenate 'string
					       (string #\Newline)
					       "conclusion:"
					       (string #\Newline)
					       (string #\Tab)
					       "addq $"
					       (write-to-string stack*)
					       ", "
					       "%rsp"
					       (string #\Newline)
					       (string #\Tab)
					       "popq %rbp"
					       (string #\Newline)
					       (string #\Tab)
					       "retq")))
		  (concatenate 'string prelude assembly conclusion))))))))))

(defun compile-code (exp filename)
  (with-open-file (str filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format str (ast->x86 (select exp)))))
