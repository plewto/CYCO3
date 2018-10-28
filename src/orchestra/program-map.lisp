;;;; CYCO3 src/orchestra/program-map
;;;;
;;;; Program maps are functions of form
;;;;
;;;;   (lambda time &key bank program)
;;;;
;;;; Which return a list of MIDI events:
;;;;   ((time-1 . midi-event-1)
;;;;    (time-2 . midi-event-2)
;;;;     .....................
;;;;    (time-n . midi-event-n))
;;;;
;;;; The program argument has a few special values:
;;;;
;;;;    program = keyword :doc     -> display documentation and return empty list.
;;;;    program = keyword :default -> use instrument's default program-number.
;;;;    program = integer p        -> use program p, assuming it is a valid MIDI program number.
;;;;    program = something-else   -> dependent on the specific map function.
;;;;    program = nil              -> empty list


(defun null-program-map (time &key bank program)
  "A Dummy program-map used as a place-holder. 
It does not generate any events."
  (dismiss time bank)
  (if (eq program :doc)
      (format t "NULL-PROGRAM-MAP ~~ Does not create MIDI events.~%"))
  nil)

(defun set-basic-program-map (instrument &key (offset 0)(min 0)(max 127))
  "Sets a basic program-map for instrument.
The program map has a range of program to which it responds. Program numbers
outside of this range are ignored. 
The offset argument is typically 0 or 1 to match the lowest displayed program 
number of the instrument.  IE the lowest program number on a Yamaha DX7 is 1 
which corresponds to an actual MIDI value of 0.  On the other hand then lowest 
program for an Oberheim Matrix 1000 is 0.

If the program-map sees a program number of :default it uses the program-number
property of the instrument."

  (flet ((docfn ()
		(format t "Instrument ~A Basic Program Map~%" (name instrument))
		(format t "    offset: ~A   min: ~A   :max: ~A" offset min max)
		nil)
	 (warnfn (pnum)	
		 (cyco-warning
		  (sformat "Instrument ~A does not recognize program ~A"
			   (name instrument) pnum)
		  nil)))
    (let ((fn #'(lambda (time &key bank program)
		  (dismiss bank)
		  (cond ((eq program :doc)
			 (docfn))
			((integerp program)
			 (let ((pnum (+ offset program)))
			   (if (and (<= min pnum)(<= pnum max))
			       (list (cons time (midi-program-change
						 (channel-index instrument)
						 (limit pnum 0 127))))
			     (warnfn pnum))))
			((eq program :default)
			 (list (cons time (midi-program-change
					   (channel-index instrument)
					   (program-number instrument)))))
			(t nil)))))
      (program-map! instrument fn)
      fn)))

(defun set-symbolic-program-map (instrument htab &key (offset 0))
  "Sets symbolic program-map for instrument.
A symbolic program-map converts symbols to program numbers.
Whenever the program-map sees the program number :default, it uses the 
program-number property of the instrument.

Map entry format has the form 
    ((cons name-1 program-number-1 name [optional-remarks])
     (cons name-2 program-number-2 name [optional-remarks])
      ....)"
  (if (alist-p htab)(setf htab (alist->hash-table htab (length htab))))
  (flet ((docfn ()
		(format t "Instrument ~A Symbolic Program Map~%" (name instrument))
		(let ((acc '()))
		  (maphash #'(lambda (a b)
			       (push (cons (->string a) b) acc))
			   htab)
		  (dolist (v (sort acc #'(lambda (a b)
					   (< (car (cdr a))(car (cdr b))))))
		    (format t "    [~16A] -> ~3D ~A~%"
			    (car v)
			    (car (cdr v))
			    (or (third (cdr v)) ""))))
		-1)
	 (warnfn (pnum)
		 (cyco-warning
		  (sformat "Instrument ~A does not recognize program ~A"
			   (name instrument) pnum))
		 nil))
    (let ((fn #'(lambda (time &key bank program)
		  (dismiss bank)
		  (let* ((tabval (gethash program htab))
			 (pnum (cond ((eq program :doc)
				      (docfn))
				     (tabval
				      (car tabval))
				     ((integerp program)
				      (+ offset program))
				     ((eq program :default)
				      (program-number instrument))
				     (t nil))))
		    (cond ((= pnum -1)
			   nil)
			  ((integerp pnum)
			   (list (cons time (midi-program-change
					     (channel-index instrument)
					     pnum))))
			  (t (warnfn pnum)))))))
      (program-map! instrument fn)
      fn)))

