;;;; PigIron CYCO patterns/pattern.lisp
;;;;
;;;; PATTERN
;;;;  |
;;;;  +-- LINE
;;;;  +-- CYCLE
;;;;  +-- BAG
;;;;  +-- DICE
;;;;  +-- COIN
;;;;  +-- INSTRUMENT-LAYER
;;;;

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
   "PATTERN is the super class for all other pattern types."))

(defmethod pattern-p ((obj t)) nil)

(defmethod pattern-p ((obj pattern)) obj)

(defmethod clone ((p pattern) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let ((rs (make-instance (type-of p) :of (clone (elements p)))))
    (reset rs)
    rs))
		  
(defmethod reset ((p pattern))
  (setf (value p) nil
	(pointer p) 0)
  (dolist (e (elements p))
    (reset e))
  p)

(defmethod cardinality ((lst list))
  (length lst))

(defmethod cardinality ((v vector))
  (length v))

(defmethod cardinality ((p pattern))
  (cardinality (elements p)))

(defmethod remaining ((p pattern))
  (let ((ptr (pointer p))
	(mx (cardinality p)))
    (and ptr (- mx ptr))))

(defmethod next-1 ((obj t)) obj)

(defmethod next-1 ((p pattern))
  ;;(cyco-error (sformat "NEXT-1 not defined for ~A" (type-of p)))) ; :ERROR pattern not-implemented
  (cyco-not-implemented-error 'next-1 P))

  
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

(defmethod ->string ((p pattern))
  (sformat "~A :of ~A" (type-of p)(elements p)))

(defmethod ->pattern ((pat pattern) &key ptype)
  (dismiss ptype)
  pat)

(defmethod ->pattern ((obj t) &key (ptype 'cycle))
  (make-instance ptype :of (->list obj)))

(defmethod transpose ((pat pattern)(x integer))
  (setf (elements pat)(transpose (elements pat) x))
  pat)

(defmethod invert ((pat pattern)(pivot t))
  (setf (elements pat)(invert (elements pat) pivot))
  pat)

(defmethod retrograde ((p pattern))
  (setf (elements p)(retrograde (elements p)))
  p)
