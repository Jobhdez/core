(in-package #:zetta)

(defstruct atomic-assignment
  temp-var
  n)

(defstruct atomic-sum
  lexp
  rexp)

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
	  (make-atomic-assignment :temp-var (generate-temp-name "temp_")
				  :n (make-neg-num :num n)))
	 ((py-sum :lexp e1 :rexp e2)
	  (make-atomic-sum :lexp (remove-complex e1)
			   :rexp (remove-complex e2)))
	 ((py-assignment :name name
			 :exp e)
	  (let* ((temp-exp (get-temp-var e))
		 (atomic (get-atomic e))
		 (atomic-exp1 (make-py-assignment :name name
						  :exp (make-atomic-sum :lexp atomic
									:rexp (atomic-assignment-temp-var temp-exp))))
		 (temp-name (generate-temp-name "temp_")))
	    (setf (gethash name *temp-names*) name)
	    (list temp-exp atomic-exp1)))
	 ((py-print :exp e)
	  (let* ((hash-values (hash-table-values *temp-names*))
		(hash-keys   (hash-table-keys *temp-names*))
		 (last-key (car (last hash-keys)))
		 (temp-name (generate-temp-name "temp_"))
		 (hash-value (gethash last-key *temp-names*)))
	    (progn (remhash last-key *temp-names*)
		   (make-atomic-assignment :temp-var temp-name
				           :n (make-atomic-sum :lexp hash-value
						              :rexp e)))))
	 (_ (error "no valid expression."))))

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
		(make-atomic-assignment :temp-var (generate-temp-name "temp_")
					:n e)
		(make-atomic-assignment :temp-var (generate-temp-name "temp_")
					:n e2))))))

(defun get-atomic (exp-node)
  (match exp-node
	 ((py-sum :lexp e :rexp e2)
	  (when (or (py-constant-p e)
		    (py-constant-p e2))
	    (if (py-constant-p e)
		e
		e2)))))

	 
