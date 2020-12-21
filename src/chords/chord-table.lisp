;;;; CYCO chords chord-table.lisp
;;;;
;;;; Defines table for mapping symbolic names to chord structure.
;;;;

(in-package :cyco)

(defclass chord-table (abstract-chord-model)
  ((templates
    :type hash-table
    :reader chord-table-templates
    :initform (make-hash-table :size 36)
    :initarg :templates))
  (:documentation
   "Default implementation of ABSTRACT-CHORD-MODEL"))
   

(defmethod define-chord ((chord-table chord-table)
			 (name symbol)
			 (template list)
			 &optional (description ""))
  (if (every #'keynumber-p template)
      (progn 
	(setf (gethash name (chord-table-templates chord-table)) template)
	(setf (gethash name (chord-table-descriptions chord-table)) (->string description))
	template)
    (cyco-type-error 'define-chord "List of keynumbers" template)))

(defmethod defines-chord-p ((chord-table chord-table)(name symbol))
  (bool (gethash name (chord-table-templates chord-table))))

(defmethod chord-types ((chord-table chord-table))
  (let ((acc '()))
    (maphash #'(lambda (a b)
		 (declare (ignore b))
		 (push a acc))
	     (chord-table-templates chord-table))
    (sort acc #'(lambda (a b)
		  (string< (->string a)(->string b))))))

(defmethod dump-chords ((chord-table chord-table))
  (format t "~A Chords:~%" (name chord-table))
  (dolist (cname (chord-types chord-table))
    (format t "  ~12A ~24A ~A~%"
	    cname
	    (gethash cname (chord-table-templates chord-table))
	    (gethash cname (chord-table-descriptions chord-table)))))

(defmethod chord-template ((chord-table chord-table)(name symbol)(keynumber t))
  "keynumber argument is ignored."
  (declare (ignore keynumber))
  (or (gethash name (chord-table-templates chord-table))
      (and
       (progn 
	 (cyco-warning
	  (sformat "Undefined Chord: ~A" name)
	  "Using default (0)")
	 '(0)))))
