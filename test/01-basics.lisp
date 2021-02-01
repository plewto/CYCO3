;; test 01-basics


(constant +test-constant+ 1)
(constant +test-constant+ 2)
(pass? "01-basics constant 1.1"
       (= +test-constant+ 1) "Constant value reassigned.")

(let ((counter 10))
  (while (plusp counter)
    (setf counter (1- counter)))
  (pass? "01-basics while 1.2"
	 (zerop counter) ""))
      
(constant-function test-constant-function :a-constant)
(dolist (arg '(nil t 1 2 3))
  (pass? "01-basics constant function 1.3"
	 (eq (funcall #'test-constant-function arg) :a-constant) ""))
