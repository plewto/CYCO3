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
      acc))

  (defun get-controller-number (name &key (default nil))
    (or (gethash (->symbol name) controller-table)
	(progn
	  (cyco-error
	   (sformat "Undefined controller ~A, using default ~A" name default))
	  default))))
    
(define-controller 'bank-select  0)
(define-controller 'wheel  1)
(define-controller 'breath  2)
(define-controller 'foot  4)
(define-controller 'port-time  5)
(define-controller 'volume  7)
(define-controller 'pan  10)
