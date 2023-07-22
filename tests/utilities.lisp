(in-package #:zetta-tests)

(defun run-zetta-tests ()
  (run-package-tests
   :packages '(:zetta-tests)
   :interactive t))
