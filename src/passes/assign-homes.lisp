(in-package #:zetta)

(defparameter *varnames* (make-hash-table :test 'equal))

(defvar *variable* 4)

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
 (flatten (mapcar (lambda (instr) (assignh  instr)) (flatten (mapcar (lambda (n) (patch-instructions n)) instructions)))))
					 
(defun assignh (instr)
  (match instr
	 ((instruction :name name :arg1 a1 :arg2 a2)
	  (cond ((and (numberp a1)
		      (atomic-var-p a2))
		 (if (gethash a2 *varnames*)
		     (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*))
		     (progn (assign-stack instr)
			    (make-instruction :name name
					      :arg1 a1
				              :arg2 (gethash a2 *varnames*)))))

		((and (immediate-p a1)
		      (atomic-var-p a2))
		 (if (gethash a2 *varnames*)
		     (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*))
		     (progn (assign-stack instr)
			    (make-instruction :name name
					      :arg1 a1
				              :arg2 (gethash a2 *varnames*)))))
		 


		((and (atomic-var-p a1)
		      (stringp a2))
		 (if (gethash a1 *varnames*)
		     (make-instruction :name name
				       :arg1 (gethash a1 *varnames*)
				       :arg2 a2)
		     (progn (assign-stack instr)
			    (make-instruction :name name
				       :arg1 (gethash a1 *varnames*)
				       :arg2 a2))))

		((and (stringp a1)
		      (atomic-var-p a2))
		 
		  (if (gethash a2 *varnames*)
		     (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*))
		     (progn (assign-stack instr)
			    (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*)))))
			    

		((and (atomic-var-p a1)
		      (symbolp a2))
		 (if (gethash a1 *varnames*)
		     (make-instruction :name name
				       :arg1 (gethash a1 *varnames*)
				       :arg2 a2)
		     (progn (assign-stack instr)
			    (make-instruction :name name
				       :arg1 (gethash a1 *varnames*)
				       :arg2 a2))))

		((and (py-constant-p a1)
		      (atomic-var-p a2))
		 (if (gethash a2 *varnames*)
		     (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*))
		     (progn (assign-stack instr)
			    (make-instruction :name name
					      :arg1 a1
					      :arg2 (gethash a2 *varnames*)))))

		((and (py-var-p a1)
		      (atomic-var-p a2))
		 (if (gethash a2 *varnames*)
		      (make-instruction :name name
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*))
		     (progn (assign-stack instr)
			    (make-instruction :name name
					      :arg1 a1
					      :arg2 (gethash a2 *varnames*)))))
		     
		 

		(t (error "Not valid Instruction."))))

	 ((callq :label lbl)
	  instr)))

(defun assign-stack (instr)
  (match instr
	 ((instruction :name name :arg1 a1 :arg2 a2)
	  (cond ((and (numberp a1)
		      (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))

		((and (immediate-p a1)
		      (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
			
		
		((and (atomic-var-p a1)
		      (stringp a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a1 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
		((and (stringp a1)
		      (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))

		((and (atomic-var-p a1)
		      (symbolp a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a1 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))

		((and (py-constant-p a1)
		      (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
		((and (py-var-p a1)
		      (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
		(t (error "Not valid intr for assigning stack."))))))

		 

		
		       
				    
		       


		

		
  
	        
  
