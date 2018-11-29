;;;; CYCO
;;;; 

(defclass chord-table (abstract-chord-model)
  ((templates
    :type hash-table
    :reader chord-table-templates
    :initform (make-hash-table :size 36)
    :initarg :templates))
  (:documentation
   "Default implementation of ABSTRACT-CHORD-MODEL"))
   

(defmethod define-chord ((model chord-table)
			 (name symbol)
			 (template list)
			 &optional (description ""))
  (if (every #'keynumber-p template)
      (progn 
	(setf (gethash name (chord-table-templates model)) template)
	(setf (gethash name (chord-table-descriptions model)) (->string description))
	template)
    (cyco-type-error 'define-chord "List of keynumbers" template)))

(defmethod defines-chord-p ((model chord-table)(name symbol))
  (bool (gethash name (chord-table-templates model))))

(defmethod chord-types ((model chord-table))
  (let ((acc '()))
    (maphash #'(lambda (a b)
		 (dismiss b)
		 (push a acc))
	     (chord-table-templates model))
    (sort acc #'(lambda (a b)
		  (string< (->string a)(->string b))))))

(defmethod dump-chords ((model chord-table))
  (format t "~A Chords:~%" (name model))
  (dolist (cname (chord-types model))
    (format t "  ~12A ~24A ~A~%"
	    cname
	    (gethash cname (chord-table-templates model))
	    (gethash cname (chord-table-descriptions model)))))

(defmethod chord-template ((cm chord-table)(name symbol) &optional variation)
  "Optional variation argument is ignored."
  (dismiss variation)
  (or (gethash name (chord-table-templates cm))
      (and
       (progn 
	 (cyco-warning
	  (sformat "Undefined Chord: ~A" name)
	  "Using default (0)")
	 '(0)))))
		     


