(in-package #:python-server)

(defvar *server*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 4243))

(defun start-server ()
  (hunchentoot:start *server*))

(defun stop-server ()
  (hunchentoot:stop *server*))


(defparameter +format-string+
  #+(or win32 mswindows windows)
  "explorer ~S"
  #+(or macos darwin)
  "open ~S"
  #-(or win32 mswindows macos darwin windows)
  "xdg-open ~S")

(defun open-browser (url)
  "Opens the browser to your target url"
  (uiop:run-program (format nil +format-string+ url)))
"""
(defun launch ()
  (start-server)
  (uiop:run-program
   (format nil
           +format-string+
           http://localhost:4243/compile%3Fexp%3Dif%202%3D%3D2%3A%20x%3D%2030%20%2B%20%20-10%20print(x%20%2B%20%2010)%20else%3A%20y%20%3D%2034%20%2B%20-2%20print(y))))
"""
(defmacro define-route (url view-fn compile-fn ht)
  `(define-easy-handler (,view-fn :uri ,url) (exp)
     (setf (content-type*) "application/json")
     (setf (header-out "Access-Control-Allow-Origin") "*")
     (setf *show-lisp-errors-p* t)
     (let* ((,ht (make-hash-table :test 'equal))
	    (expr (,compile-fn exp)))
       (setf (gethash 'expression ,ht) (write-to-string expr))
       (stringify ,ht))))

(defun compile-py (e)
  (assign-homes
   (clean
    (select-instructions
     (remove-complex-operands
      (parse-with-lexer (token-generator (lex-line e))
			*python-grammar*))))))

(defun compile-to-monadic (e)
  (remove-complex-operands
   (parse-with-lexer (token-generator (lex-line e))
		     *python-grammar*)))

(defun compile-to-select (e)
  (clean
   (select-instructions
    (remove-complex-operands
     (parse-with-lexer
      (token-generator
       (lex-line e))
      *python-grammar*)))))

(define-route "/compile" comp compile-py ht)
(define-route "/compile-mon" cmon compile-to-monadic monht)
(define-route "/compile-select" csel compile-to-select sht)
