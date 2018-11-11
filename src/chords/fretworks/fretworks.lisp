;;;; CYCO chords/fretworks/fretworks
;;;;

(defclass fret-model (chord-model)
  ((name
    :type symbol
    :reader name
    :initarg :name)
   (fret-count
    :type integer
    :reader fret-count
    :initform nil
    :initarg :frets)
   (strings
    :type vector
    :reader fm-string-array
    :initform nil
    :initarg :strings)
   (chord-table
    :type hash-table
    :reader chord-table
    :initform (make-hash-table)) ))

(defmethod fm-string-count ((fw fret-model))
  (length (fm-string-array fw)))

(defmethod ->string ((fw fret-model))
  (let ((acc (sformat "FRET-MODEL(~A)  ~A ("(fret-count fw)(name fw)))
	(sarray (fm-string-array fw)))
    (dotimes (i (fm-string-count fw))
      (let ((r (monochord-root (aref sarray i))))
	(setf acc (str+ acc (sformat "~A " (keyname r))))))
    (str+ acc ")")))

(defmethod chord-p ((fw fret-model)(obj t)) nil)

(defmethod chord-p ((fw fret-model)(chord-type symbol))
  (gethash chord-type (chord-table fw)))

(defmethod chord-family ((fw fret-model)(chord-type symbol))
  (or (chord-p fw chord-type)
      (cyco-error (sformat "Chord type ~A is not defined for FRET-MODEL ~A"
			   chord-type (name fw)))))

(defmethod chord-types ((fw fret-model))
  (let ((acc '()))
    (maphash #'(lambda (q r)
		 (dismiss r)
		 (push (->string q) acc))
	     (chord-table fw))
    (sort acc #'(lambda (a b)(string< a b)))))

;; (defmethod ?chords ((fw fret-model))
;;   (dolist (q (chord-types fw))
;;     (format t "~A~%" q)))

;; If chord-type already defined, reuse it.
(defmethod add-chord-type! ((fw fret-model)(chord-type symbol))
  (let ((cf (or (chord-p fw chord-type)
		(make-instance 'chord-family :chord-type chord-type))))
    (setf (gethash chord-type (chord-table fw)) cf)
    cf))

(flet ((illegal-positions-list (fw key positions)
			      (let ((msg1 "Illegal fret position list")
				    (msg2 "FRET-MODEL: ~A   key: ~A")
				    (msg3 "Method: CHORD-VARIATION!")
				    (msg4 "positions list/vector was:  ~A"))
				(cyco-error msg1 (sformat msg2 (name fw) key)
					    msg3 (sformat msg4 positions)))))
  (defmethod chord-variation! ((fw fret-model)(chord-type symbol)(key t)(positions vector))
    (let* ((cf (or (chord-p fw chord-type)
		   (add-chord-type! fw chord-type)))
	   (sarray (fm-string-array fw))
	   (scount (length sarray))
	   (acc (make-array scount)))
      (if (not (= scount (length positions)))(illegal-positions-list fw key positions))
      (dotimes (i scount)
	(let* ((pos (aref positions i))
	       (kn (mc-position->keynumber (aref sarray i) pos)))
	  (if (eq kn :ERROR)(illegal-positions-list fw key pos))
	  (setf (aref acc i) kn)))
      (add-chord-family-variation! cf key (->list acc)) )))

(defmethod chord-variation! ((fw fret-model)(chord-type symbol)(key t)(positions list))
  (chord-variation! fw chord-type key (->vector positions)))

;; (defmethod chord-variation ((fw fret-model)(chord-type symbol)(key t))
;;   (let ((cf (chord-family fw chord-type)))
;;     (->list (get-chord-variation cf key))))

(defmethod chord ((fw fret-model)(chord-type symbol)(key t))
  (let ((cf (chord-family fw chord-type)))
    (->list (get-chord-variation cf key))))


(defmethod fm-key-positions ((fw fret-model)(key t) &key (no-duplicates t))
  "Returns nested list of fret positions of string which produce key.
If no-duplicates is true (the default) duplicate keys are removed.
The result has the form ((:string s :keynumber k :position p)
                         (:string s :keynumber k :position p)
                          ...................................)
Where s is string number 
      k is keynumber (octave equivalents to key)
      p is fret position."

  (let ((acc '())
	(sarray (fm-string-array fw)))
    (dotimes (i (length sarray))
      (let ((mc (aref sarray i)))
	(dolist (p (fm-key-positions mc key))
	  (let ((kn (mc-position->keynumber mc p)))
	    (push (list :string i :keynumber kn :position p) acc)))))
    (if no-duplicates
	(remove-duplicates (reverse acc) :test #'(lambda (q r)
						   (= (fourth q)
						      (fourth r))))
      (reverse acc))))

;; (defmethod ? ((fw fret-model))
;;   (format t "~A~%" (->string fw))
;;   (format t "Chord types: ~A~%" (chord-types fw))
;;   (maphash #'(lambda (q r)
;; 	       (format t "~A~%" q)
;; 	       (? r))
;; 	   (chord-table fw)))

(flet ((define-solo-chords (fw key)
	 (let ((scount (fm-string-count fw))
	       (positions (fm-key-positions fw key :no-duplicates t)))
	   (dolist (p positions)
	     (let ((str (second p))
		   (pos (sixth p))
		   (plist (make-array scount :initial-element 'X)))
	       (setf (aref plist str) pos)
	       (chord-variation! fw :solo key plist))))))

  (defun fret-model (name fret-count strings)
    (let ((acc '()))
      (dolist (s strings)
	(push (make-instance 'monochord
			     :frets fret-count
			     :root (keynumber s))
	      acc))
      (let ((fw (make-instance 'fret-model
			       :name (->symbol name)
			       :frets fret-count
			       :strings (->vector (reverse acc)))))
	(dolist (key '(c cs d ds e f fs g gs a as b))
	  (define-solo-chords fw key))
      fw))))

;; Sorts chord variations by average keynumber, ignoring rest.
;;
(defmethod sort-fret-model! ((fw fret-model))
  (maphash #'(lambda (q r)
	       (dismiss q)
	       (sort-chord-family! r))
	   (chord-table fw)))
	   
  
(defmethod define-chord-family! ((fw fret-model) (chord-type symbol) &key
				 c cs d ds e f fs g gs a as b df ef gf af bf)
  (if (or (and cs df)(and ds ef)(and fs gf)(and gs af)(and as bf))
      (let ((msg1 "DEFINE-CHORD-FAMILY!  FRET-MODEL ~A")
	    (msg2  "Mutually exclusive enharmonic key arguments set."))
	(cyco-warning (sformat msg1 (name fw)) msg2)))
  (add-chord-type! fw chord-type)
  (dotimes (pc 12)
    (let ((data (aref (vector c (or cs df) d (or ds ef) e f (or fs gf) 
			      g (or gs af) a (or as bf) b) pc)))
      (dolist (template (->list data))
	(chord-variation! fw chord-type pc template))))
  fw)

(defmethod format-neck-position ((fm fret-model)(pos integer)
				 &key (select '(0 1 2 3 4 5 6 7 8 9 10 11)))
  "Returns string representation of single neck position.
   fm - A FRET-MODEL
   pos - INTEGER, fret position
   select - list of pitch classes to display, all other pitches are left blank."
            
  (let ((acc (sformat "~2d - " pos))
	(carray (fm-string-array fm)))
    (dotimes (i (length carray))
      (let* ((mchord (aref carray i))
	     (root (monochord-root mchord))
	     (key (+ root pos))
	     (pclass (pitch-class key))
	     (keyrep (if (some #'(lambda (x)(= x pclass)) select) (keyname key) " - ")))
	(setf acc (sformat "~a ~3a " acc keyrep))))
    acc))
	     
(defmethod format-neck ((fm fret-model)
			&key (select '(0 1 2 3 4 5 6 7 8 9 10 11)))
  "Returns string representation of instrument neck.
   fm - A FRET-MODEL
   select - list of pitch classes to display, all other pitches are left blank."
  (let ((acc "   "))
    (dotimes (n (length (fm-string-array fm)))
      (setf acc (sformat "~a~4d " acc n)))
    (setf acc (sformat "~a~%" acc))
    (dotimes (pos (fret-count fm))
      (setf acc (sformat "~a~a~%" acc (format-neck-position fm pos :select select))))
    acc))
