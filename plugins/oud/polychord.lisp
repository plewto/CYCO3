;;;; cyco-oud polychord
;;;;
;;;; Extends ABSTRACT-CHORD-MODEL using suit of monochords.
;;;;

(defclass polychord (abstract-chord-model)
  ((fret-count
    :type integer
    :reader polychord-fret-count
    :initform 12
    :initarg :frets)
   (capo-position
    :type integer
    :accessor capo-position
    :initform 0
    :initarg :capo)
   (strings
    :type vector			; of monochords
    :accessor polychord-strings
    :initarg :strings)
   (chord-table
    :type hash-table			; of chord-family
    :reader chord-table
    :initform (make-hash-table :size 8))))

(defun make-polychord (name fret-count tuning)
  "Creates new instance of POLYCHORD.
name - symbol
fret-count - integer
tuning - list of open string tunings as absolute key-numbers."
  (let ((acc '()))
    (dolist (kn (->list tuning))
      (push (make-monochord :root-key (keynumber kn)
			    :fret-count fret-count
			    :capo-position 0)
	    acc))
    (make-instance 'polychord
		   :name (->cyco-symbol name)
		   :frets fret-count
		   :capo 0
		   :absolute t
		   :strings (->vector (reverse acc)))))
    
(defmethod polychord->string ((plychrd polychord) &key (frets nil))
  "Returns string representation of instrument neck."
  (let ((acc "    ||")
	(bcc "----++")
	(ccc "")
	(mchords (polychord-strings plychrd)))
    (setf bcc (str+ bcc (scopies (1+ (polychord-fret-count plychrd)) "--+")))
    (dotimes (i (1+ (polychord-fret-count plychrd)))
      (setf acc (str+ acc (cond ((member i '(3 5 7 9 15 17 19 21)) "* |")
				((member i '(12 24)) "**|")
				(t (sformat "~2D|" i))))))
    (dotimes (i (length mchords))
      (setf ccc (str+ ccc (sformat "~A~%" (monochord->string (aref mchords i)
							     (nth i frets))))))
    (sformat "~A~%~A~%~A~%" acc bcc ccc)))

(defmethod ->string ((plychrd polychord))
  (sformat "~A~%~A" (name plychrd)(polychord->string plychrd)))

(defmethod string-count ((plychrd polychord))
  (length (polychord-strings plychrd)))

(defmethod defines-chord-p ((plychrd polychord)(chord-type symbol))
  (gethash (->cyco-symbol chord-type)
	   (chord-table plychrd)))

(defmethod chord-types ((plychrd polychord))
  (let ((acc '()))
    (maphash #'(lambda (k v)
		 (dismiss v)
		 (push k acc))
	     (chord-table plychrd))
    (sort acc #'string<)))

(labels ((chord-spec-error
	  (message plychrd chord-type key)
	  (cyco-error
	   "POLYCHORD error"
	   message
	   (sformat "POLYCHORD ~A  chord ~A  key ~A"
		    (name plychrd) chord-type key)))

	 (string-count-test
	  (plychrd chord-type key fret-positions)
	  (or (= (string-count plychrd)
		 (length fret-positions))
	      (chord-spec-error "Chord specification does not match string count"
				plychrd chord-type key)))

	 ;; Returns minimum integer value in list, ignores non-integer items.
	 (fret-min
	  (template)
	  (apply #'min (remove nil template :test #'(lambda (a b)
						      (dismiss a)
						      (not (integerp b))))))
	 ;; Returns maximum integer value in list, ignores non-integer items.		       
	 (fret-max
	  (template)
	  (apply #'max (remove nil template :test #'(lambda (a b)
						      (dismiss a)
						      (not (integerp b))))))		  
	 (get-or-create-chord-family
	  (plychrd chord-type)
	  (let* ((sym (->cyco-symbol chord-type))
		 (family (gethash sym (chord-table plychrd))))
	    (if (not family)
		(progn
		  (setf family (make-chord-family sym))
		  (setf (gethash sym (chord-table plychrd)) family)))
	    family))

	 ;; Creates chord-templates from fret-position list.
	 ;; The template is added to the appropriate chord-family.
	 ;;
	 ;; spec ~ (capo (pos1 pos2 pos3 ...))
	 ;;
	 ;;   capo - fret position of capo
	 ;;   pos1, pos2, pos3, ... fret positions relative to capo
	 ;;   for each string.  Position may be negative.
	 ;;
	 (parse-fret-positions
	  (plychrd chord-type key spec)
	  (let* ((family (get-or-create-chord-family plychrd chord-type))
		 (capo (car spec))
		 (positions (second spec))
		 (monochords (polychord-strings plychrd))
		 (acc '()))
	    (if (string-count-test plychrd chord-type key positions)
		(progn
		  (setf positions (->vector (mapcar #'(lambda (q)
							(cond ((symbol-eq-p q 'X) -1)
							      ((symbol-eq-p q 'O) capo)
							       (t (+ q capo))))
						    positions)))
		  (dotimes (i (string-count plychrd))
		    (let* ((pos (aref positions i))
			   (mc (aref monochords i))
			   (kn (monochord-keynumber mc pos)))
		      (push kn acc)))
		  (add-chord-variation family key (reverse acc))))))

	 ;; Creates templates for each chord variation.
	 ;; One, and only one, of spec1 and spec2 must be non-nil.
	 ;; spec1 and spec2 specify enharmonic versions of the same chord.
	 ;; The specification format is a nested list:
	 ;;
	 ;;  ((capo (fret-positions ...))
	 ;;   (capo (fret-positions ...))
	 ;;    ........................
	 ;;   (capo (fret-positions ...)))
	 ;;
	 (process-chord-specs
	  (plychrd chord-type key spec1 &optional spec2)
	  (if (and spec1 spec2)
	      (progn
		(chord-spec-error "Enharmonic chord specifications"
				  plychrd chord-type key)
		(return-from process-chord-specs)))
	  (dolist (spec (or spec1 spec2))
	    (parse-fret-positions plychrd chord-type key spec)))

	 (merge-description
	  (plychrd chord-type text)
	  (setf chord-type (->cyco-symbol chord-type))
	  (if text 
	      (let ((txt (gethash chord-type (chord-table-descriptions plychrd))))
		(setf txt (if txt
			      (setf txt (sformat "~A~%~A" txt text))
			    text))
		(setf (gethash chord-type (chord-table-descriptions plychrd)) txt))))
	 
	 )
  
  (defmethod define-chord-family ((plychrd polychord)
  				  (chord-type symbol)
  				  &key
				  a as bf b
				  c cs df d
				  ds ef e f
				  fs gf g gs af
				  description)
    "Defines finger positions for each keey of specific chord-type.
Only specify one enharmonic key.  That is -only- use one of the keywords
from each list below.
    :AS :BF
    :CS :DF
    :DS :EF
    :FS :GF
    :GS :AF
    :AS :BF"
    (setf chord-type (->cyco-symbol chord-type))
    (process-chord-specs plychrd chord-type 'a a)
    (process-chord-specs plychrd chord-type 'as as bf)
    (process-chord-specs plychrd chord-type 'b b)
    (process-chord-specs plychrd chord-type 'c c)
    (process-chord-specs plychrd chord-type 'cs cs df)
    (process-chord-specs plychrd chord-type 'd d)
    (process-chord-specs plychrd chord-type 'ds ds ef)
    (process-chord-specs plychrd chord-type 'e e)
    (process-chord-specs plychrd chord-type 'f f)
    (process-chord-specs plychrd chord-type 'fs fs gf)
    (process-chord-specs plychrd chord-type 'g g)
    (process-chord-specs plychrd chord-type 'gs gs af)
    (merge-description plychrd chord-type description))

  (defmethod define-movable-chord ((plychrd polychord)
				   (chord-type symbol)
				   (starting-position integer)
				   (starting-key t)
				   (template list)
				   &key (description nil))
    "Defines a set of chords using a 'movable' bar-chord template."
    (let* (;; (min-offset (fret-min template))
	   ;; (min-starting-position (+ starting-position min-offset))
	   (max-offset (fret-max template))
	   (max-position (- (polychord-fret-count plychrd)
			    (+ max-offset starting-position)))
	   (current-key starting-key)
	   (pos starting-position)
	   (capo 0)
	   (keys #(c cs d ds e f fs g gs a as b)))
      ;; (if (< starting-position min-starting-position)
      ;; 	  (chord-spec-error
      ;; 	   "DEFINE-MOVEABLE-CHORD initial position too low."
      ;; 	   plychrd chord-type starting-key))
      (merge-description plychrd chord-type description)
      (while (<= pos max-position)
	(process-chord-specs plychrd chord-type
	  		     (aref keys (pitch-class current-key))
	  		     (list (list capo template)))
	(setf current-key (1+ current-key))
	(setf pos (1+ pos))
	(setf capo (1+ capo)))))

  )

(defmethod dump-chords ((plychrd polychord))
  (format t "POLYCHORD ~A~%" (name plychrd))
  (dolist (ctype (chord-types plychrd))
    (let ((descript (gethash ctype (chord-table-descriptions plychrd)))
	  (family (gethash ctype (chord-table plychrd))))
      (if descript
	  (progn
	    (format t "Descriptions:~%")
	    (format t "~A~%" descript)))
      (dump-chords family))))

(defmethod remove-duplicate-chords ((plychrd polychord))
  (dolist (ctype (chord-types plychrd))
    (let ((family (gethash ctype (chord-table plychrd))))
      (remove-duplicate-chords family))))

(defmethod chord-template ((plychrd polychord)(chord-type t)(keynumber t))
  (let* ((family (or (gethash chord-type (chord-table plychrd))
		     (let ((message (sformat "chord-model ~A does not define chord type: ~A"
					     (name plychrd) chord-type)))
		       (cyco-error message)))))
    (chord-variant family keynumber)))

