(in-package #:core-tests)

(defun run-core-tests ()
  (run-package-tests
   :packages '(:core-tests)
   :interactive t))
