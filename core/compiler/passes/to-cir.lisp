(in-package :core)

(defstruct if-goto-loop
  cnd body)

(defun to-cir (anf)
  (let ((blocks (make-hash-table :test 'equalp))
	(counter 0))
    (labels ((toc (instr)
	       (match instr
		 ((while-atomic :loop-block blk :test-block tblk :pre-block pblk)
		  (let ((loop-block (concatenate 'string "loop_" (write-to-string (setf counter (+ counter 1))))))
		    (progn
		      (setf (gethash loop-block blocks) (toc tblk))
		      (setf (gethash loop-block blocks) (mapcar (lambda (e) (toc e)) blk))
		      (let ((goto* (make-if-goto-loop :cnd (toc tblk) :body (concatenate 'string "goto " loop-block))))
			(flatten
			 (list (mapcar (lambda (e) (toc e)) pblk)
			       goto*
			       blocks))))))
		 (_ instr))))
      (mapcar (lambda (instr) (toc instr)) anf))))
		      
	 
