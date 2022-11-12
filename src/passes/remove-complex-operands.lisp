(in-package #:zetta)

(defstruct atomic-assignment
  temp-var
  n)

(defstruct atomic-sum
  lexp
  rexp)

(defstruct atomic-var
  name)

(defun remove-complex-operands (parse-tree)
  "Removes complex operands. Eg each subexpression of a binary-op needs to be
   a atomic expression which is an integer or variable. complex operations
  get assigned to a variable; an example of a complex expression is -10."
  (if (listp (py-module-statements parse-tree))
      (flatten (mapcar (lambda (node) (remove-complex node)) (flatten (py-module-statements parse-tree))))))

(defparameter *temp-names* (make-hash-table))

(defun remove-complex (parse-node)
  "Given a parse tree node it removes the complex expression."
  ;;(defparameter *temp-names* (make-hash-table))
  (match parse-node
	 ((py-constant :num n)
	  (make-py-constant :num n))
	 ((py-neg-num :num n)
	  (make-atomic-assignment :temp-var (make-atomic-var :name (generate-temp-name "temp_"))
				  :n (make-py-neg-num :num n)))
	 ((py-sum :lexp e1 :rexp e2)
	  (if (and (py-constant-p e1) (py-constant-p e2))
	      (list (make-py-sum :lexp e1 :rexp e2))
	      (when (or (py-neg-num-p e2) (py-neg-num-p e1))
		(cond ((py-neg-num-p e2)
		       (let* ((rmv-complex (remove-complex e2))
			      (tmp-var (atomic-assignment-temp-var rmv-complex)))
			 (list rmv-complex
		               (make-atomic-sum :lexp (remove-complex e1)
			                        :rexp tmp-var))))
		      ((py-neg-num-p e1)
		       (let* ((rmv-complex (remove-complex e1))
		              (tmp-var (atomic-assignment-temp-var rmv-complex)))
			 (list rmv-complex
			       (make-atomic-sum :lexp tmp-var
			                        :rexp (remove-complex e2)))))))))
		   
	 ((py-assignment :name name
			 :exp e)
	  (cond ((py-constant-p e)
		 (make-py-assignment :name (make-atomic-var :name name)
				     :exp e))
		((py-sum-p e)
		 (cond ((positive-sum-p e)
			e)
		       
		       ((variable-sum-p e)
			(make-py-assignment :name (make-atomic-var :name name) :exp e))
		       
		       ((negative-sum-p e)
			(let* ((temp-exp (get-temp-var e))
		               (atomic (get-atomic e))
		               (atomic-exp1 (make-py-assignment :name (make-atomic-var :name name)
						                :exp (make-atomic-sum :lexp atomic
									              :rexp
										      (atomic-assignment-temp-var
										       temp-exp))))
			       (temp-name (generate-temp-name "temp_")))
			  (setf (gethash name *temp-names*) name)
	                  (list temp-exp atomic-exp1)))
		       (t (error "No more valid sum expressions."))))

		((py-constant-p e)
		 e)))
		
	  ((py-print :exp e)
	   (cond ((py-var-p e)
		  (let ((e* (make-py-var :name (make-atomic-var :name (py-var-name e)))))
		    (make-py-print :exp e*)))
		 
		 ((var-num-sum-p e)
		  (let* ((temp-name (generate-temp-name "temp_"))
		         (var-name (py-sum-lexp e))
		         (num (py-sum-rexp e)))
		    (list (make-atomic-assignment :temp-var (make-atomic-var :name temp-name)
				                  :n (make-atomic-sum :lexp (make-atomic-var :name var-name)
						                      :rexp num))
			  (make-py-print :exp (make-atomic-var :name temp-name)))))

		 (t (error "No other print expressions."))))
	  (_ (error "no valid expression."))))

(defun positive-sum-p (node)
  "Checks whether NODE is a sum expression composed of positive numbers."
  (match node
	 ((py-sum :lexp e1 :rexp e2)
	  (if (and (py-constant-p e1)
		   (py-constant-p e2))
	      t
	      nil))))

(defun variable-sum-p (node)
  "Checks whether NODE is a sum expression composed of variables. eg y + z"
  (match node
	 ((py-sum :lexp e :rexp e2)
	  (if (and (py-var-p e)
		   (py-var-p e2))
	      t
	      nil))))

(defun negative-sum-p (node)
  "checks if NODE is a sum expression composed of a positive and negative number. eg 10 + -3"
  (match node
	 ((py-sum :lexp e :rexp e2)
	  (if (and (py-constant-p e)
		   (py-neg-num-p e2))
	      t
	      nil))))

(defun var-num-sum-p (node)
  "checks if node is a sum expression composed of a constant and variable. eg x + 10"
  (match node
	 ((py-sum :lexp e :rexp e2)
	  (if (and (py-var-p e)
		   (py-constant-p e2))
	      t
	      nil))))

(defvar gensym-count 0)

(defun generate-temp-name (prefix-name)
  (progn (setf gensym-count (+ gensym-count 1))
	 (concatenate 'string
		      prefix-name
		      (write-to-string gensym-count))))

 (defun get-temp-var (exp-node)
  (match exp-node
	 ((py-sum :lexp e :rexp e2)
	  (when (or (py-neg-num-p e)
		    (py-neg-num-p e2))
	    (if (py-neg-num-p e)
		(make-atomic-assignment :temp-var (make-atomic-var :name (generate-temp-name "temp_"))
					:n e)
		(make-atomic-assignment :temp-var (make-atomic-var :name (generate-temp-name "temp_"))
					:n e2))))))

(defun get-atomic (exp-node)
  (match exp-node
	 ((py-sum :lexp e :rexp e2)
	  (when (or (py-constant-p e)
		    (py-constant-p e2))
	    (if (py-constant-p e)
		e
		e2)))))

	 
