(in-package #:zetta)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun build-module (statements)
  (make-py-module :statements (flatten statements)))

(defun build-print (print-token left-paren-token exp right-paren-token)
  (declare (ignore print-token left-paren-token right-paren-token))
  (make-py-print :exp exp))

(defun build-while (while-token exp colon-tok statements)
  (declare (ignore while-token colon-tok))
  (make-py-while :exp exp :statements statements))

(defun build-if (if-token exp colon-tok statement else-tok colon2-tok statement2)
  (declare (ignore if-token colon-tok else-to colon2-tok))
  (make-py-if :exp exp :if-statement statement :else-statement statement2))

(defun build-neg-num (minus-token exp)
  (declare (ignore minus-token))
  (make-py-neg-num :num exp))

(defun build-addition (exp plus-token exp2)
  (declare (ignore plus-token))
  (make-py-sum :lexp exp :rexp exp2))

(defun build-sub (exp neg-token exp2)
  (declare (ignore neg-token))
  (make-py-sub :lexp exp :rexp exp2))

(defun build-bool (bool)
  (make-py-bool :bool bool))

(defun build-bool-op (exp bool-op exp2)
  (make-py-bool-op :lexp exp :op bool-op :rexp exp2))

(defun build-unaryop (unaryop exp)
  (make-py-unary :op unaryop :exp exp))

(defun build-cmp (exp cmp exp2)
  (make-py-cmp :lexp exp :cmp cmp :rexp exp2))

(defun build-variable (var)
  (make-py-var :name var))

(defun build-assignment (name assignment-token exp)
  (declare (ignore assignment-token))
  (make-py-assignment :name name :exp exp))

(defun build-int (num)
  (make-py-constant :num num))

(define-parser *python-grammar*
    (:start-symbol module)
  (:terminals (:boolop :unaryop :cmp :bool :assignment :if :else :while :colon :name :constant :right-paren :left-paren :print :plus :minus))
  (module
   (statements #'build-module))
  (statements
   statement
   exp
   (statement statements)
   (exp statements))
  (statement
   (:print :left-paren :right-paren)
   (:print :left-paren exp :right-paren #'build-print)
   exp
   assignment
   (:while exp :colon statements #'build-while)
   (:if exp :colon statement :else :colon statement #'build-if))
  (exp
   variable
   int
   (:minus exp #'build-neg-num)
   (exp :plus exp #'build-addition)
   (exp :minus exp #'build-sub)
   (:bool #'build-bool)
   (exp :boolop exp #'build-bool-op)
   (:unaryop exp #'build-unaryop)
   (exp :cmp exp #'build-cmp))
  (variable
   (:name #'build-variable))
  (assignment
   (:name :assignment exp #'build-assignment))
  (int
   (:constant #'build-int))))
  

