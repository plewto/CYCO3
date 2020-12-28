;;;; CYCO controller-number.lisp
;;;;
;;;; Defines symbolic controller numbers.
;;;; May be depreciated.
;;;;

(in-package :cyco)

(let ((controller-table (make-hash-table :size 128)))

  (defun define-controller (name controller-number)
    (setf (gethash (->symbol name) controller-table) controller-number))

  (defun defined-controllers ()
    (let ((acc '()))
      (maphash #'(lambda (k v)
		   (push (cons k v) acc))
	       controller-table)
      (sort acc #'(lambda (a b)(< (cdr a)(cdr b))))))

  (defun ?controllers ()
    (format t "Symbolic MIDI controller names~%")
    (dolist (ctrl (defined-controllers))
      (if (not (string= (->string (car ctrl))(->string (cdr ctrl))))
	  (format t "    ~12A --> ~3D~%" (car ctrl)(cdr ctrl)))))

  
  (defun get-controller-number (name &key (default nil))
    (or (gethash (->symbol name) controller-table)
	(progn
	  (cyco-error
	   (sformat "Undefined controller ~A, using default ~A" name default))
	  default))))


(defun initialize-controller-numbers ()
  (loop for n from 0 below 128
	do (define-controller n n))
  (define-controller 'bank-select  0)
  (define-controller 'wheel  1)
  (define-controller 'breath  2)
  (define-controller 'foot  4)
  (define-controller 'port-time  5)
  (define-controller 'volume  7)
  (define-controller 'pan  10))

(initialize-controller-numbers)
