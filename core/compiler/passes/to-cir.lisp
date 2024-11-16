(in-package :core)

(defstruct if-goto-loop
  cnd body blocks)

(defstruct if-goto
  cnd thn els blocks)

(defstruct goto
  block)

(defun to-cir (anf)
  (let ((blocks (make-hash-table :test 'equalp))
	(counter 0))
    (labels ((toc (instr)
	       (match instr
		 ((while-atomic :loop-block blk :test-block tblk :pre-block pblk)
		  (let ((loop-block (concatenate 'string "loop_" (write-to-string (setf counter (+ counter 1))))))
		    (progn
		      (setf (gethash loop-block blocks) (mapcar (lambda (e) (toc e)) blk))
		      (let ((goto* (make-if-goto-loop :cnd (toc tblk) :body (make-goto :block loop-block) :blocks blocks)))
			(flatten
			 (flatten
			  (list (mapcar (lambda (e) (toc e)) pblk)
			        goto*)))))))
		      
		 ((if-atomic :thn thn :els els :cnd cnd)
		  (let ((blk-thn (concatenate 'string "block_" (write-to-string (setf counter (+ counter 1)))))
			(blk-els (concatenate 'string "block_" (write-to-string (setf counter (+ counter 2))))))
		    (progn
		      (setf (gethash blk-thn blocks) (if (listp thn) (mapcar (lambda (e) (toc e)) thn) thn))
		      (setf (gethash blk-els blocks) (if (listp els) (mapcar (lambda (e) (toc e)) els) els))
		      (make-if-goto :cnd (toc cnd) :thn (make-goto :block blk-thn) :els (make-goto :block blk-els) :blocks blocks))))
		      
		 (_ instr))))
	    
      (flatten (mapcar (lambda (instr) (toc instr)) anf)))))
		      
	 
