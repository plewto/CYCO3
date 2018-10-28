;;;; CYCO3 src/patterns/pattern
;;;;
;;;; A Pattern is an object which generates a sequence of values in some
;;;; prescribed manner.  Patterns may be nested to any level.  Two of the
;;;; common pattern types are LINE and CYCLE.
;;;;
;;;; A Line returns it's elements in sequence until the final element is
;;;; reached. Thereafter the line continues to return the final element.
;;;;
;;;; (line :of '(A B C)) --> A B C C C C ...
;;;;
;;;; A Cycle also returns elements in sequence.  Unlike a line, once the
;;;; final cycle element is reached, the cycle begins again from the
;;;; beginning.
;;;;
;;;; (cycle :of '(A B C)) --> A B C A B C A B ...
;;;;
;;;; Cycles and lines (and most all of the other pattern types) may be embedded
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
;;;;
;;;; PATTERN
;;;;  |
;;;;  +-- LINE
;;;;  +-- CYCLE
;;;;  +-- BAG    - random without replacement
;;;;  +-- DICE   - random with replacement
;;;;  +-- COIN   - random binary choice, unlike dice, coin may call functions
;;;;  +-- INSTRUMENT-LAYER - special pattern type used for layering instruments.
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
   "PATTERN is the base class for all other pattern types."))

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

