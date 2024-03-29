;;;; CYCO pattern pattern.lisp
;;;;
;;;; A Pattern is an object which generates a sequence of values in some
;;;; prescribed manner.  Patterns may be nested to any level.  Two of the
;;;; common pattern types are LINE and CYCLE.
;;;;
;;;; A Line returns its elements in sequence until the final element is
;;;; reached. Thereafter the line continues to return the final element.
;;;;
;;;; (line :of '(A B C)) --> A B C C C C ...
;;;;
;;;; A Cycle also returns elements in sequence.  Unlike a line, once the
;;;; final element is reached, the cycle begins again from the beginning.
;;;;
;;;; (cycle :of '(A B C)) --> A B C A B C A B ...
;;;;
;;;; Cycles and lines (and most other pattern types) may be embedded
;;;; into each other.
;;;;
;;;; (cycle :of (list (cycle :of '(1 2))(cycle :of '(A B C))))
;;;; --> 1 A 2 B 1 C 2 A 1 B 2 C ...
;;;;
;;;; (cycle :of (list (line :of '(1 2 3)) (cycle :of '(A B C))))
;;;; --> 1 A 2 B 3 C 3 A 3 B 3 C 3 A ...
;;;;
;;;; (line :of (list 1 2 3 (cycle :of '(A B C))))
;;;; --> 1 2 3 A B C A B C A B C ...
;;;;

(in-package :cyco)

(defclass pattern nil
  ((elements
    :type list
    :accessor elements
    :initform '()
    :initarg :of)
   (value
    :type t
    :accessor value
    :initform nil)
   (pointer
    :type integer
    :accessor pointer
    :initform 0))
  (:documentation
   "PATTERN is the base class for all other pattern types."))

(defmethod pattern-p ((obj pattern)) obj)

(defmethod clone ((mother pattern) &key &allow-other-keys)
  (let ((daughter (make-instance (type-of mother) :of (clone (elements mother)))))
    (reset daughter)
    daughter))
		  
(defmethod reset ((p pattern))
  (dolist (e (elements p))
    (reset e))
  (setf (value p)(value (car (elements p)))
	(pointer p) 0)
  p)

(defmethod pattern-length ((p pattern) &key &allow-other-keys)
  (length (elements p)))

(defmethod remaining ((p pattern))
  (let ((ptr (pointer p))
	(mx (pattern-length p)))
    (and ptr (- mx ptr))))

(defmethod next-1 ((obj t)) obj)

(defmethod next-1 ((p pattern))
  (cyco-type-error 'next-1 '?pattern p))
  
(defmethod next-n ((obj t)(n integer))
  (copies n obj))

(defmethod next-n ((obj pattern)(n integer))
  (let ((acc '()))
    (dotimes (q n)
      (push (next-1 obj) acc))
    (reverse acc)))

(defmethod next ((p pattern) &optional (n nil))
  (cond
   ((eq n :all)(elements p))
   ((eq n :rest)
    (let ((q (remaining p)))
      (and q (next-n p q))))
   ((integerp n)
    (next-n p n))
   (t (next-1 p))))

(defmethod next ((obj t) &optional (n nil))
  (if (and (numberp n)(plusp n))
      (copies (truncate n) obj)
    obj))

;; (defmethod ->string ((p pattern))
;;   (sformat "~A :of ~A" (type-of p)(elements p)))

(defmethod ->string ((p pattern))
  (let ((acc (sformat "(~A :of (" (type-of p))))
    (loop for e in (elements p) do
	  (setf acc (str+ acc " " (->string e))))
    (setf acc (str+ acc "))"))
    acc))


(defmethod ->pattern ((pat pattern) &key pattern-type)
  (declare (ignore pattern-type))
  pat)

(defmethod ->pattern ((obj t) &key (pattern-type 'cycle))
  (make-instance pattern-type :of (->list obj)))

(defmethod transpose ((pat pattern)(x integer))
  (setf (elements pat)(transpose (elements pat) x))
  pat)

(defmethod invert ((pat pattern)(pivot t))
  (setf (elements pat)(invert (elements pat) pivot))
  pat)

(defmethod retrograde ((p pattern))
  (setf (elements p)(retrograde (elements p)))
  p)

