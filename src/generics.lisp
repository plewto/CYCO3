;;;; CYCO3 src/generics
;;;;


(defgeneric ? (obj))
(defmethod ? ((obj t))
  (format t "~A~%" (type-of obj)))
  

(defgeneric project-p (obj))
(defmethod project-p ((obj t)) nil)
(defmethod project-p ((obj null)) nil)



(defgeneric section-p (obj))
(defgeneric part-p (obj))
(defgeneric group-p (obj))

(defgeneric parent (obj))

;; render to intermediate form
;;    ((time-0 midi-event)
;;     (time-1 midi-event)
;;      .................)
;;
;; Result should be sorted by time and then priority.
;;
(defgeneric render-once (obj &key offset))

(defgeneric render-n (obj count &key offset))

(defgeneric priority (obj))

;; ---------------------------------------------------------------------- 
;;			    Sequence related

(defgeneric ->list (obj)
  (:documentation
   "Coerce argument to a list.
If argument is not a non-string sequence, embed it in a list."))

(defgeneric ->vector (obj)
  (:documentation
   "Coerce argument to a vector.
If argument is not a non-string sequence, embed it in a vector."))

(defgeneric slice (seq start &optional end)
  (:documentation
   "Returns sequence slice ala Python."))

(defgeneric cnth (n seq)
  (:documentation
   "Return nth element of sequence in circular manner.
(cnth 2 '(a b c)) --> C
(cnth 3 '(a b c)) --> A"))

(defgeneric final (obj)
  (:documentation
   "Returns final element from object"))

(defgeneric butfinal (obj)
  (:documentation
   "Returns all but final element from object."))

(defgeneric alist-p (obj)
  (:documentation
   "Predicate returns true is argument is an association list.
The following conditions must be met:
1) obj is a list.
2) every element of obj is a cons."))

(defmethod alist-p ((obj t)) nil)

(defmethod alist-p ((lst list))
  (every #'consp lst))

;; ---------------------------------------------------------------------- 
;;				   CLONE

(defgeneric clone (src &key new-name new-parent)
  (:documentation
   "Returns cloned copy of source object.
If the object type has the concept of name or parent, then new-name
and new-parent may be used to alter the result.
new-name is a format string where ~A may be used to include the original 
name."))

(defmethod clone ((obj t) &key new-name new-parent)
  "Default clone action is to return it's argument."
  (dismiss new-name new-parent)
  obj)

(defmethod clone ((src hash-table) &key new-name new-parent)
  (dismiss new-name new-parent)
  (let ((other (make-hash-table :size (hash-table-count src))))
    (maphash #'(lambda (key val)
		 (setf (gethash key other) (clone val)))
	     src)
    other))

(defmethod clone ((c cons) &key new-name new-parent)
  (dismiss new-name new-parent)
  (cons (clone (car c))(clone (cdr c))))


(defmethod clone ((v vector) &key new-name new-parent)
  (dismiss new-name new-parent)
  (->vector (clone (->list v))))

;; ---------------------------------------------------------------------- 
;;				   NAME

(defgeneric name (obj)
  (:documentation
   "Returns name of object."))

(defmethod name ((obj t)) nil)

(defmethod name ((s symbol)) (symbol-name s))

(defmethod name ((s string)) s)

(defgeneric name! (obj new-name))

(defgeneric remarks (obj))

(defgeneric remarks! (obj text))


;; ---------------------------------------------------------------------- 
;;			       CHANNEL-INDEX

(defgeneric channel-index (obj)
  (:documentation
   "Returns channel-index of object.
For MIDI the 'channel-index' is the actual byte value, 
an integer between 0 and 15 inclusive."))

(defmethod channel-index ((n null)) 0)

(defmethod channel-index ((channel integer))
  (if (and (plusp channel)(<= channel 16))
      (1- channel)
    (format t "WARNING: Invalid MIDI channel ~A.~%" channel)))

;; ---------------------------------------------------------------------- 
;;			    CHANNEL & CHANNEL!

(defgeneric channel (obj &optional resolve))
 
(defmethod channel ((n null) &optional resolve)
  (dismiss resolve)
  1)

;; (defgeneric channel! (obj value)
;;   (:documentation
;;    "Sets objects MIDI channel."))


;; ---------------------------------------------------------------------- 
;;				 Programs

(defgeneric program-number (obj))
(defgeneric program-bank (obj))
(defgeneric program-map (obj))
(defgeneric program-map! (obj fn))


;; ---------------------------------------------------------------------- 
;;				  RESET

;; 'reset' clashes with SBCL
;;

(defgeneric reset (obj)
  (:documentation
   "Resets object to initial state.
Returns object."))

(defmethod reset ((obj t))
  "The default reset action is to do nothing and return the argument."
  obj)

;; ---------------------------------------------------------------------- 
;;				 ->STRING

(defgeneric ->string (obj)
  (:documentation
   "Returns string representation of argument."))

(defmethod ->string ((obj t))(format nil "~A" obj))

(defmethod ->string ((s string)) s)

;; Do not use on list of MIDI events.
;; try dump-events instead
(defmethod ->string ((lst list))
   (mapcar #'->string lst))

;; ---------------------------------------------------------------------- 
;;				 ->SYMBOL

(defgeneric ->symbol (arg)
  (:documentation
   "Creates new symbol using name of argument arg.
If (symbolp arg) is true, return arg."))

(defmethod ->symbol ((s symbol)) s)

(defmethod ->symbol ((s string))
  ;;(make-symbol (string-upcase s)))
  (intern (string-upcase s)))
  

(defmethod ->symbol ((n number))
  (->symbol (format nil "~A" n)))

;; ---------------------------------------------------------------------- 
;;			    TRANSPOSE & INVERT

(defgeneric transpose (obj x)
  (:documentation
   "Transpose obj by x half-steps"))


(defmethod transpose ((obj t) x)
  "Default transpose action is to do nothing and return the argument."
  obj)

(defgeneric invert (obj pivot)
  (:documentation
   "Performs interval inversion of argument around pivot key."))

(defmethod invert ((obj t) pivot)
  "Default invert action is to do nothing and return the argument."
  obj)


(defgeneric retrograde (obj))

(defmethod retrograde ((obj t)) obj)
(defmethod retrograde ((seq sequence))(reverse seq))

;; ---------------------------------------------------------------------- 
;;				PALINDROME

(defgeneric palindrome (obj &key elide)
  (:documentation
   "Returns palindrome of object.
elide determine how end-points are treated.
  nil    - (default)  ABC  --> ABCCBA
  :first - ABC --> ABCCB
  :last  - ABC --> ABCBA
  :both  - ABC --> ABCB"))

(defmethod palindrome ((obj t) &key elide)
  "Default palindrome action is to return a clone of the argument."
  (dismiss elide)
  (clone obj))

;; ---------------------------------------------------------------------- 
;;			       Keynumbers

(defgeneric keynumber-p (obj)
  (:documentation
   "Predicate true if argument can be treated as a keynumber."))

(defgeneric rest-p (obj)
  (:documentation "Predicate true if argument can be treated as a rest"))

(defgeneric keynumber (obj)
  (:documentation
   "Converts argument to MIDI keynumber or list of keynumbers."))

(defgeneric octave (obj)
  (:documentation
   "Returns octave number of argument as keynumber. 
Rest and all negative values have octave -1."))

(defgeneric pitch-class (obj)
  (:documentation
   "Returns pitch class of argument as keynumber.
The pitch class is an integer in {0,1,2,...,11}
Rest and all negative values have pitch class -1."))

(defgeneric keyname (obj)
  (:documentation
   "Returns symbolic name for obj as keynumber.
Where there are two or more inharmonic names with the same value 
the 'sharp' value is returned."))


;; ---------------------------------------------------------------------- 
;;			      Dynamic values

(defgeneric dynamic-p (obj)
  (:documentation
   "Predicate, true if argument may be treated as a dynamic value."))

(defgeneric dynamic (obj)
  (:documentation
   "Converts argument to numeric dynamic value."))

(defgeneric dynamic-name (obj)
  (:documentation
   "Returns symbolic name of dynamic value.
Some rounding error may occur with numeric arguments."))   

(defgeneric dynamic->velocity (dy)
  (:documentation
   "Converts dynamic value to MIDI velocity."))

;; ---------------------------------------------------------------------- 
;;			       Metric values

(defgeneric metric-p (obj)
  (:documentation
   "Predicate, true if obj is a valid metric value."))

(defgeneric metric (obj)
  (:documentation
   "Converts obj to numeric metric value.
It is an error if obj can not be converted."))

(defgeneric metric-expression-p (exp)
  (:documentation
   "Predicate, nil if argument is invalid metric-expression.
It the argument is a valid expression, returns float result of
evaluating the expression."))

(defgeneric metric-expression (exp)
  (:documentation
   "Evaluates metric-expression
It is an error if expression is invalid."))


;;; ---------------------------------------------------------------------- 
;;;			    Pick and Permutation


(defgeneric pick (obj)
  (:documentation
   "Selects element at random from argument."))

(defmethod pick ((obj t))
  "The default pick action is to return the argument."
  obj)

(defmethod pick ((n integer))
  "Returns random integer between 0 (inclusive) and n (exclusive)."
  (random n))


(defgeneric permute (obj)
  (:documentation "Returns permutation on argument."))

(defmethod permute ((obj t))
  "The default permute action is to do nothing and return the argument."
  obj)


(defgeneric duration (obj))

(defmethod duration ((n number))
  (float n))

(defgeneric period (obj))

(defmethod period ((obj t))
  (duration obj))

(defgeneric dump-events (obj &key range filter render))


;; pad-end in seconds
(defgeneric ->smf (obj &key filename offset repeat pad))
  

