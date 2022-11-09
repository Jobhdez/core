(in-package #:zetta)

(defparameter *varnames* (make-hash-table :test 'equal))

(defvar *variable* 4)

(defun assign-homes (instructions)
  (mapcar (lambda (instr) (assignh instr)) instructions))

(defun assignh (instr)
  (match instr
	 ((instruction :name n :arg1 a1 :arg2 a2)
	  (cond ((and (atomic-var-p a1) (not (symbolp a2)))
		 (if (gethash a1 *varnames*)
		    (make-instruction :name n
				      :arg1 (gethash a1 *varnames*)
				      :arg2 a2)
		    (progn (assign-stack instr)
			   (make-instruction :name n
				      :arg1 (gethash a1 *varnames*)
				      :arg2 a2))))
			   
		((and (atomic-var-p a2) (not (symbolp a1)))
		 (if (gethash a2 *varnames*)
		     (make-instruction :name n
				      :arg1 a1
				      :arg2 (gethash a2 *varnames*))
		 (progn (assign-stack instr)
			(make-instruction :name n
					   :arg1 a1
					   :arg2 (gethash a2 *varnames*)))))
		((and (symbolp a1) (symbolp a2))
		 (make-instruction :name n
				   :arg1 a1
				   :arg2 a2))
		
		((and (numberp a1) (stringp a2))
		 (make-instruction :name n
				   :arg1 a1
				   :arg2 a2))
		
		((and (stringp a1) (stringp a2))
		 (make-instruction :name n
				   :arg1 a1
				   :arg2 a2))
		
		((and (atomic-var-p a1) (symbolp a2))
		 (if (gethash a1 *varnames*)
		     (make-instruction :name n
				       :arg1 (gethash a1 *varnames*)
				       :arg2 a2)
		     (progn (assign-stack instr)
			    (make-instruction :name n
					      :arg1 (gethash a1 *varnames*)
					      :arg2 a2))))
		((and (numberp a1) (atomic-var-p a2))
		 (if (gethash a2 *varnames*)
		     (make-instruction :name n
				       :arg1 a1
				       :arg2 (gethash a2 *varnames*)
		     (progn (assign-stack instr)
			    (make-instruction :name n
					      :arg1 a1
					      :arg2 (gethash a2 *varnames*))))))))
		
		
	 ((callq :label l)
	  (make-callq :label l))
	 (_ (error "Instruction is not valid."))))
	        

(defun assign-stack (instr)
  (match instr
	 ((instruction :name n :arg1 a1 :arg2 a2)
	  (cond ((and (atomic-var-p a1) (not (stringp a2)))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a1 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
		
		((and (atomic-var-p a2) (not (stringp a1)))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))
		
		((and (stringp a1) (atomic-var-p a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))

		((and (atomic-var-p a1) (symbolp a2))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))

		
		((and (atomic-var-p a2) (numberp a1))
		 (setf *variable* (* *variable* 2))
		 (setf (gethash a2 *varnames*)
		       (concatenate 'string "-" (write-to-string *variable*) "(%rbp)")))))))

	
		
				   
  
