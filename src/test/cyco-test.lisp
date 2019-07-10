;;;; CYCO3 test/cyco-test
;;;;

(in-package :cyco)


;; NOTE: same-thing-p does not handle multi-dimensional matrixes
;;
(defgeneric same-thing-p (a b))

(defmethod same-thing-p ((a t)(b t))
  (eq a b))

(defmethod same-thing-p ((a list)(b t))
  (if (and a b
	   (listp b)
	   (= (length a)(length b))
	   (same-thing-p (car a)(car b)))
      (same-thing-p (cdr a)(cdr b))
    (eq a b)))
      
(defmethod same-thing-p ((a vector)(b t))
  (if (and a b
	   (vectorp b)
	   (= (length a)(length b)))
      (let ((result t))
	(dotimes (i (length a))
	  (setf result (same-thing-p (aref a i)(aref b i)))
	  (if (not result)
	      (return-from same-thing-p nil)))
	result)
    nil))


(defun ~= (a b &optional (max-diff 0.00001))
  (< (abs (- a b)) max-diff))
  


(defun none (predicate list)
  "Returns true if predicate is false for all list elements."
  (not (some predicate list)))

(defun load-test (test-name)
  (format t "~%")
  (banner1 test-name)
  (ld (join-path "src" "test" test-name :as-file)))

(defun pass (test-name)
  (format t "OK ~A~%" test-name))

(defun fail (test-name &optional remarks)
  (format t "~%*** FAIL ~A ***~%" test-name)
  (if remarks
      (format t "*** REMARKS: ~A~%" remarks)))

(defun pass? (flag test-name &optional remarks)
  (if flag
      (pass test-name)
    (fail test-name))
  (if remarks
      (format t "REMARKS: ~A~%" remarks)))

(defun not-tested (name)
  (format t "* Not Tested: ~A~%" name))

(load-test "test-basics")
(load-test "test-math-utilities")
(load-test "test-seq-utilities")
(load-test "test-string-utilities")
(load-test "test-dynamics")
(load-test "test-keynumbers")
(load-test "test-metrics")
(load-test "test-node")
(load-test "test-chords")
(load-test "test-midi-util")
