;;;; PigIron cyco orchestra program-map
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
  (dismiss time bank)
  (if (eq program :doc) (format t "NULL-PROGRAM-MAP ~~ Does not create MIDI events.~%"))
  nil)

(defun set-basic-program-map (instrument &key (offset 0)(min 0)(max 127))
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


;; map entry format:
;;    (cons  name . (program-number name [remarks]))
;;
(defun set-symbolic-program-map (instrument htab &key (offset 0))
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
