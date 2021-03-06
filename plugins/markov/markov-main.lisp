;;;; CYCO pattern markov.lisp
;;;;
;;;; The MARKOV pattern selects values from a random path.
;;;;
;;;; Defines Markov Chain in terms of a Pattern.
;;;; Before a chain is defined a graph must be created of markov-links,
;;;; where a link has a value and a list of potential new markov-link.
;;;; The number of occurrence of a new link determines its probability of
;;;; being the next value.
;;;;
;;;; Once the graph has been defined it may be wrapped in a MARKOV-CHAIN
;;;; Pattern.  The resulting object may be used as any other pattern type.
;;;;

(in-package :cyco)

(defgeneric ->markov-link (object)
  (:documentation "Coerce object to a markov-link"))


(def-type-predicate markov-chain-p)
(def-type-predicate markov-link-p)

(defgeneric markov-add-link (source destination weight)
  (:documentation
   "Adds potential destinations for a Markov-link.
source - the markov-link
destination - potential next value.  The destination may be of any type
but is coerced to a markov-link.
weight - The number of times destination is added to sources' links list."))

(defgeneric markov-walk (markov-link)
  (:documentation
   "Returns the next markov-link."))

(defclass markov-link nil
  ((value
    :type t
    :accessor value
    :initform nil
    :initarg :value)
   (links
    :type list
    :accessor markov-links
    :initform nil))
  (:documentation
   "Defines an individual value of a markov chain. The value may be of any
  type including a general Pattern.

The links field is a list of potential next-values. Each element is also a
markov-link and the relative number of occurrences determines the probability 
of each potential value."))

(defmethod markov-link-p ((object markov-link)) t)

(defmethod ->markov-link ((object markov-link)) object)

(defmethod ->markov-link ((object t))
  (make-instance '(markov-link :value object)))
  

(defmethod markov-add-link ((source markov-link)
			    (destination markov-link)
			    (weight integer))
  (setf (markov-links source)
	(append (markov-links source)
		(copies weight destination))))

(defmethod markov-add-link ((source markov-link)
			    (destination t)
			    (weight integer))
  (markov-add-link source (->markov-link destination) weight))

(defmethod markov-walk ((link markov-link))
  (pick (markov-links link)))

(defun markov-link (value &rest ilinks)
  "Constructs new markov-link
value - The links 'value', may be of any type.
ilinks - potential destinations, elements must have the form (value weight)"
  (let ((node (make-instance 'markov-link :value value)))
    (dolist (lnk ilinks)
      (markov-add-link node (car lnk)(second lnk)))
    node))

(let ((seen '()))
  (labels ((new-item (mkv)
		     (not (member mkv seen)))
	   (_reset (mkv)
		   (reset (value mkv))
		   (push mkv seen)
		   (dolist (c (markov-links mkv))
		     (if (new-item c)
			 (_reset c))))
	   (_transpose (mkv x)
		       (transpose (value mkv) x)
		       (push mkv seen)
		       (dolist (c (markov-links mkv))
			 (if (new-item c)
			     (_transpose c x))))
	   (_invert (mkv pivot)
		    (invert (value mkv) pivot)
		    (push mkv seen)
		    (dolist (c (markov-links mkv))
		      (if (new-item c)
			  (_invert c pivot)))) )
		    
    (defmethod reset ((mkv markov-link))
      (setf seen '())
      (_reset mkv))

    (defmethod transpose ((mkv markov-link)(x t))
      (if x
	  (progn 
	    (setf seen '())
	    (_transpose mkv x)))
      mkv)

    (defmethod invert ((mkv markov-link)(pivot t))
      (if pivot
	  (progn 
	    (setf seen '())
	    (_invert mkv pivot)))
      mkv)))

(constant +NULL-MARKOV-LINK+ (markov-link nil))

;; ----------------------------------------------------------

(defclass markov-chain (pattern)
  ((root
    :type markov-link
    :accessor markov-root
    :initform +NULL-MARKOV-LINK+
    :initarg :root)
   (current
    :type markov-link
    :accessor markov-current
    :initform +NULL-MARKOV-LINK+
    :initarg :root))
  (:documentation
   "Wraps graph of markov-link objects in a Pattern."))

(defmethod markov-chain-p ((object markov-chain)) t)

(defmethod reset ((chain markov-chain))
  (setf (markov-current chain)
	(markov-root chain))
  (reset (markov-root chain))
  chain)
    
(defmethod value ((chain markov-chain))
  (value (markov-current chain)))

(defmethod next-1 ((chain markov-chain))
  (let ((crnt (markov-walk (markov-current chain))))
    (prog1
	(next-1 (value crnt))
      (setf (markov-current chain) crnt))))

(defun markov-chain (root)
  (if (markov-link-p root)
      (make-instance 'markov-chain :root root)
    (cyco-type-error 'markov-chain 'markov-link root
		     "Root argument to MARKOV-CHAIN must be instance of MARKOV-LINK")))

(defmethod transpose ((mkv markov-chain)(n t))
  (transpose (markov-root mkv) n))

(defmethod invert ((mkv markov-chain)(pivot t))
  (invert (markov-root mkv) pivot))

      
