;;;; CYCO fretworks fretted-chord-model
;;;;

(defclass fretted-chord-model (abstract-chord-model)
  ((minimum-octave
    :type integer
    :reader minimum-octave
    :initform 0
    :initarg :min-octave)
   (fret-count
    :type integer
    :reader fret-count
    :initform nil
    :initarg :fret-count)
   (strings
    :type vector			; of monochord
    :reader monochords
    :initform nil
    :initarg :strings)
   (chord-table				; of chord-family
    :type hash-table
    :reader chord-table
    :initform (make-hash-table :size 16)))
   (:documentation
    "Defines a chord-model in terms of a fretted instrument.
The model has a set number of monochord strings of uniform fret-count.

There are potentially several variations for each chord-type of each key.
In practice a chord-type is selected, then a variation based on key 
pitch-class and octave.  Higher octaves return chords with a higher
average key-numbers.  For each octave exceeding the number of variations, 
lower-frequency notes are gshaved from the chord template."))

(defmethod string-count ((fcm fretted-chord-model))
  (length (monochords fcm)))

(defmethod defines-chord-p ((fcm fretted-chord-model)(chord-type symbol))
  (gethash (->symbol chord-type :cyco)
	   (chord-table fcm)))

(defmethod chord-types ((fcm fretted-chord-model))
  (let ((acc '()))
    (maphash #'(lambda (k v)
		 (dismiss v)
		 (push k acc))
	     (chord-table fcm))
    (sort acc #'string<)))

(defmethod chord-template ((fcm fretted-chord-model)
			   (chord-type symbol)
			   &optional variant)
  (let ((family (gethash (->symbol chord-type :cyco)(chord-table fcm))))
    (if (not variant)
	(cyco-error
	 "Expected key-number passed by optional VARIANT argument to CHORD-TEMPLATE")
      (if family
	  (chord-variant family variant)
	(cyco-warning (sformat "~A chords not defined for FRETTED-CHORD-MODEL ~A"
			       chord-type (name fcm)))))))

(defun fretted-chord-model (name frets tuning &key (minimum-octave 0))
  (let ((strings (->vector (mapcar #'(lambda (q)
				       (make-monochord :root-key (keynumber q)
						       :fret-count frets))
				   tuning))))
    (make-instance 'fretted-chord-model
		   :absolute t
		   :min-octave minimum-octave
		   :name (->symbol name :cyco)
		   :fret-count frets
		   :strings strings)))


(labels (;; Called on malformed chord specification.
	 ;;
	 (chord-spec-error
	  (message fcm chord-type key)
	  (cyco-error
	   "FRETTED-CHORD-MODEL DEFINE-CHORD-FAMILY"
	   message
	   (sformat "FRETTED-CHORD-MODEL ~A  chord ~A  key ~A"
		    (name fcm) chord-type key)))

	 ;; Checks that fret-position list matches string-count.
	 ;; 
	 (string-counts-match-test
	  (fcm chord-type key fret-positions)
	  (or (= (string-count fcm)(length fret-positions))
	      (chord-spec-error "Chord specification does not match string count."
				fcm chord-type key)))

	 (get-or-create-chord-family
	  (fcm chord-type)
	  (let* ((sym (->symbol chord-type :cyco))
		 (family (gethash sym (chord-table fcm))))
	    (if (not family)
		(progn 
		  (setf family (make-chord-family sym (minimum-octave fcm)))
		  (setf (gethash sym (chord-table fcm)) family)))
	    family))

	 ;; Creates chord-templates from fret-position list.
	 ;; The template is added to the appropriate chord-family.
	 ;;
	 ;; spec ~ (capo (pos1 pos2 pos3 ...))
	 ;;
	 ;;   capo - fret position of capo
	 ;;   pos1, pos2, pos3, ... fret positions relative to capo for each string.
	 ;;
	 (parse-fret-positions
	  (fcm chord-type key spec)
	  (let* ((family (get-or-create-chord-family fcm chord-type))
		 (capo (car spec))
		 (positions (->vector (second spec)))
		 (monochords (monochords fcm)))
	    (if (string-counts-match-test fcm chord-type key positions)
		(let ((acc '()))
		  (dotimes (i (string-count fcm))
		    (let* ((pos (aref positions i))
			   (mc (aref monochords i))
			   (kn (monochord-keynumber mc pos capo)))
		      (push kn acc)))
		  (add-chord-variation family key (reverse acc))))))

	 ;; Creates templates for each chord variation.
	 ;; 
	 ;; spec1 & spec2 - chord specifications as described below.
	 ;;    One, and only one, of spec1 and spec2 must be non-nil.
	 ;;    They specify the same key but are enharmonic to each other.
	 ;;    IE A-Sharp and B-flat.  Their format is a nested list:
	 ;;
	 ;;     ((capo (positions...))  variation 1
	 ;;      (capo (positions...))  variation 2
	 ;;       ..................
	 ;;      (capo (positions...)))
	 ;;
	 (process-chord-specs
	  (fcm chord-type key spec1 &optional spec2)
	  (if (and spec1 spec2)
	      (progn 
		(chord-spec-error "Enharmonic chord specifications"
				  fcm chord-type key)
		(return-from process-chord-specs nil)))
	  (dolist (spec (or spec1 spec2))
	    (parse-fret-positions fcm chord-type key spec))) )

  (defmethod define-chord-family ((fcm fretted-chord-model)
				  (chord-type symbol)
				  &key a as bf b c cs df d ds ef e f fs gf g gs af)
    "Defines a new chord-family for a fretted-chord-model
(define-chord-family fcm chord-type &key a as bf b c cs d ds ef e f fs gf g gs af)

fcm - A fretted-chord-model
chord-type - symbol name of chord, by convention chord-names are within 
square-brackets, [maj]

The keyword arguments A AS BF B C CS DF D DS EF E F FS GF G GS AF 
specify the possible variations for each key.  Several of the key arguments 
are enharmonic equivalents.  For two equivalent keys ONLY ONE may be specified.
AS==BF, CS==DF, DS==EF, FS==GF, GS==AF. 

Example:

(define-chord-family fcm '[maj]
    :a  '((capo (pos1 pos2 pos3 ...))  ;; a-major variation 1
          (capo (pos1 pos2 pos3 ...))  ;;         variation 2
           ..........................  ;;
          (capo (pos1 pos2 pos3 ...))) ;;         variation n

    :bf '((capo (pos1 pos2 pos3 ...))  ;; b-flat variations
          ..........................
          (capo (pos1 pos2 pos3 ...)))

    :c  ---
    :df ---
    :d  ---
    :ef ---
    :e  ---
    :f  ---
    :gf ---
    :g  ---

    :af '((capo (pos1 pos2 pos3 ...))
           ........................
          (capo (pos1 pos2 pos3 ...))))

The capo values are offsets to the initial fret position.
Fret positions pos1, pos2, pos3,...  
A position must be specified for each string.
Positions are one of:  integer fret position relative to capo.
                       symbol X - mute string
                       symbol O - open string"
    (process-chord-specs fcm chord-type 'a a)
    (process-chord-specs fcm chord-type 'as as bf)
    (process-chord-specs fcm chord-type 'b b)
    (process-chord-specs fcm chord-type 'c c)
    (process-chord-specs fcm chord-type 'cs cs df)
    (process-chord-specs fcm chord-type 'd d)
    (process-chord-specs fcm chord-type 'ds ds ef)
    (process-chord-specs fcm chord-type 'e e)
    (process-chord-specs fcm chord-type 'f f)
    (process-chord-specs fcm chord-type 'fs fs gf)
    (process-chord-specs fcm chord-type 'g g)
    (process-chord-specs fcm chord-type 'gs gs af)) ) 
