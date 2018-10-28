;;;; CYCO3 src/patterns/pattern-generics
;;;;
;;;;            *** DEPRECIATED ***
;;;;

#| **********************************************************
(defgeneric pattern-p (obj)(:documentation "Predicate"))
(defgeneric line-p (obj)(:documentation "Predicate"))
(defgeneric cycle-p (obj)(:documentation "Predicate"))
(defgeneric bag-p (obj)(:documentation "Predicate"))
(defgeneric dice-p (obj)(:documentation "Predicate"))

(defgeneric ->cycle (obj)
  (:documentation
   "Coerce object to a CYCLE pattern.
If obj is an instance of CYCLE,return it.
If obj is some other Pattern type, extract is elements and return as a CYCLE.
For all other cases return (CYCLE :of obj)"))

(defgeneric ->pattern (obj &key ptype)
  (:documentation
   "Coerce obj to be a PATTERN p type
If obj is already a ptype PATTERN, return is.
If obj is a non p-type pattern, extracts its elements and return a new pattern.
For all other types return (ptype :of obj)
ptype defaults to CYCLE"))

(defgeneric cardinality (obj)
  (:documentation
   "Returns the number of elements in a pattern."))

(defgeneric remaining (obj)
  (:documentation
   "Returns a count of the pattern elements which have yet to be used.
Remaining is somewhat arbitrary for certain pattern types."))

(defgeneric next-1 (obj)
  (:documentation
   "Returns the next element from pattern."))


(defgeneric next-n (obj n)
  (:documentation
   "Returns the next n elements from pattern as a list."))

(defgeneric next (obj &optional n)
  (:documentation
   "Returns the next element from a pattern.
n - nil   -> call NEXT-1 on pattern.
n = int   -> return next n elements as list.
n = :REST -> return remaining elements as a list.  
n = :ALL  -> return list of all pattern elements."))
*************************************************************** |#
