;;;; CYCO3 test/basics
;;;;

;; constant test
;;
(constant +test-constant+ 1)
(constant +test-constant+ 2)
(pass? (eq +test-constant+ 1) "constant")

(let ((counter 10))
  (while (plusp counter)
    (setf counter (1- counter)))
  (pass? (zerop counter) "while"))

(pass? (and (eq (bool nil) nil)(eq (bool 'true) t)) "bool")


(constant-function test-constant-function :a-constant)
(pass? (eq (test-constant-function 'whatever) :a-constant) "constant-function")
(pass? (true) "true")
(pass? (not (false)) "false")

(let ((current-major-version (car +cyco-version+)))
  (version current-major-version)
  (pass? t "version"))


