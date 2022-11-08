(in-package #:zetta)

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
  (flatten (mapcar (lambda (n) (select-instrs n)) (cdr ast))))

(defun select-instrs (node)
  (match node
	 ((py-assignment :name n
			 :exp e1)
	  (if (atomic-sum-p e1)
	      (let ((tmp-var (atomic-sum-rexp e1)))
		(list (make-instruction :name "movq"
				        :arg1 (make-immediate :int (py-constant-num
							    (atomic-sum-lexp e1)))
				        :arg2 n)
		      (make-instruction :name "addq"
				        :arg1 tmp-var
				        :arg2 n)))))
	 ((atomic-assignment :temp-var tmp
			     :n n)
	  (if (atomic-sum-p n)
	      (let ((vari (atomic-sum-lexp n))
		    (rexp (py-constant-num (py-sum-rexp (atomic-sum-rexp n)))))
		(list (make-instruction :name "movq"
				 :arg1 vari
				 :arg2 tmp)
		      (make-instruction :name "addq"
					:arg1 (make-immediate :int rexp)
					:arg2 tmp)))))
	 ((py-print :exp e1)
	  (list (make-instruction :name "movq"
				  :arg1 e1
				  :arg2 "%rdi")
		(make-callq :label "print_int")))))
				  
			   

(defvar gensym2-count 0)

(defun generate-temp-name2 (prefix-name)
  (progn (setf gensym2-count (+ gensym-count 1))
	 (concatenate 'string
		      prefix-name
		      (write-to-string gensym-count))))

	  
	 
