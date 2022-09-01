;;;; CYCO parts qball-transform
;;;;
;;;; Defines functions for converting QBALLs and XBALLs to STRUMMERs
;;;; TODO: Update documentation.
;;;;

(in-package :cyco-part)

(labels ((cue-format (cuelist)
		     (let* ((lengths (loop for c in cuelist
					   collect (length (sformat "~A" c))))
			    (width (apply #'max lengths)))
		       (sformat ":TIME ~~~DA " width)))
	 
	 (key-format (key-pattern cue-count)
		     (reset key-pattern)
		     (let ((mx 1))
		       (dotimes (i cue-count)
			 (dolist (key (->list (next key-pattern)))
			   (setf mx (max mx (length (->string key))))))
		       (sformat ":KEY ~~~DA " mx)))
		       
	 (general-format (param pattern cue-count)
		     (reset pattern)
		     (let ((mx 1))
		       (dotimes (i cue-count)
			 (setf mx (max mx (length (->string (next pattern))))))
		       (sformat "~A ~~~DA " param mx)))

	 (duration-format (dur-pattern cue-count)
			  (general-format ":DUR" dur-pattern cue-count))

	 (amplitude-format (dur-pattern cue-count)
			  (general-format ":AMP" dur-pattern cue-count)) )

	
        (defun qball->strummer (qball &key name instrument cuefn shuffle (stream t))
	  (reset qball)
	  (let* ((tab (scopies 3 " "))
		 (tab2 (scopies 14 " "))
		 (iname (or instrument (name (car (next (property qball :instruments))))))
		 (cuelist (next (property qball :cue-cycle) :all))
		 (first-cue (car cuelist))
		 (last-cue (final cuelist))
		 (cfrmt (cue-format cuelist))
		 (kfrmt (key-format (property qball :key-pattern) (length cuelist)))
		 (dfrmt (duration-format (property qball :articulation-pattern)(length cuelist)))
		 (afrmt (amplitude-format (property qball :dynamic-pattern)(length cuelist)))
		 (event-format (sformat "~~A(~A~A~A~A)" cfrmt kfrmt dfrmt afrmt))
		 (acc (sformat "(strummer ~A ~A~%" (or name (name qball)) iname)))
	    (reset qball)
	    (setf acc (sformat "~A~A :bars ~D~%" acc tab (bars qball)))
	    (setf acc (sformat "~A~A :cuefn ~A~%" acc tab (or cuefn "*default-cue-function")))
	    (setf acc (sformat "~A~A :shuffle ~A~%" acc tab (or shuffle "#'no-shuffle")))
	    (setf acc (sformat "~A~A :events '(" acc tab))
	    (dolist (cue cuelist)
	      (let ((dur (next (property qball :articulation-pattern)))
		    (amp (next (property qball :dynamic-pattern)))
		    (klist (->list (next (property qball :key-pattern)))))
		(loop for k in klist
		      for i from 0 do
		      (if (not (eq cue first-cue))(setf acc (sformat "~A~A" acc tab2)))
		      (setf acc (sformat event-format acc cue k dur amp))
		      (if (and (eq cue last-cue)(= i (1- (length klist))))
			  (setf acc (sformat "~A))~%" acc))
			(setf acc (sformat "~A~%" acc))))))
	    (format stream acc)))

	;; manually set chord-table, end-together
	;;
	(defun xball->strummer (xball &key name instrument cuefn shuffle (stream t))
	  (reset xball)
	  (let* ((tab (scopies 3 " "))
		 (tab2 (scopies 13 " "))
		 (iname (->string (or instrument (name (property xball :instruments)))))
		 (cuelist (next (property xball :cue-cycle) :all))
		 (cue-count (length cuelist))
		 (last-cue (final cuelist))
		 (cue-frmt (str+ (cue-format cuelist)))
		 (key-frmt (general-format ":KEY" (property xball :key-pattern) cue-count))
		 (chord-frmt (general-format ":CHORD" (property xball :chord-pattern) cue-count))
		 (dur-frmt (duration-format (property xball :articulation-pattern) cue-count))
		 (amp-frmt (amplitude-format (property xball :dynamic-pattern) cue-count))
		 (dir-frmt (general-format ":DIRECTION" (property xball :direction-pattern) cue-count))
		 (inv-frmt (general-format ":INVERSION" (property xball :inversion-pattern) cue-count))
		 (oct-frmt (general-format ":OCTAVE" (property xball :octave-pattern) cue-count))
		 (strum-frmt (general-format ":STRUM" (property xball :strum-pattern) cue-count))
		 (last-inversion -99) ;; only include these parameters 
		 (last-octave -99)    ;; they have changed.
		 (last-direction nil) ;;
		 (last-strum nil)     ;;
		 (header (str+ (sformat "(strummer ~A ~A~%" (or name (name xball)) iname)
			       (sformat "~A:bars ~D~%" tab (bars xball))
			       (sformat "~A:cuefn ~A~%" tab (or cuefn "*default-cue-function*"))
			       (sformat "~A:shuffle ~A~%" tab (or shuffle "#'no-shuffle"))))
		 (events (sformat "~A:events '((:chord [solo] :inversion 0 :octave 0 :direction down :strum 0.0)~%" tab)))
	    (reset xball)
	    (dolist (cue cuelist)
	      (let* ((key (next (property xball :key-pattern)))
		     (event (sformat "~A(~A~A" tab2 (sformat cue-frmt cue)(sformat key-frmt key)))
		     (chord (next (property xball :chord-pattern)))
		     (dur (next (property xball :articulation-pattern)))
		     (amp (next (property xball :dynamic-pattern)))
		     (inv (next (property xball :inversion-pattern)))
		     (oct (next (property xball :octave-pattern)))
		     (dir (next (property xball :direction-pattern)))
		     (strum (next (property xball :strum-pattern))))
		(setf event (str+ event
				  (sformat chord-frmt chord)
				  (sformat dur-frmt dur)
				  (sformat amp-frmt amp)))

		(if (not (= inv last-inversion))
		    (setf last-inversion inv
			  event (str+ event (sformat inv-frmt inv))))

		(if (not (= oct last-octave))
		    (setf last-octave oct
			  event (str+ event (sformat oct-frmt oct))))

		(if (not (eq dir last-direction))
		    (setf last-direction dir
			  event (str+ event (sformat dir-frmt dir))))

		(if (not (eq strum last-strum))
		    (setf last-strum strum
			  event (str+ event (sformat strum-frmt strum))))
		(setf event (str+ event ")"))
	    	(if (not (eq cue last-cue))
	    	    (setf event (sformat "~A~%" event)))
	    	(setf events (str+ events event))))
	    (format stream "~A~A))~%" header events) )))


(setf (documentation 'qball->strummer 'function)
      "Translates QBALL to readable string representing an equivalent STRUMMER.

qball       - The source QBALL.
:name       - Optional strummer name, defaults to qball's name.
:instrument - Optional strummer instrument name. defaults to first qball 
              instrument.  Note QBALL may take more then one instrument 
              while STRUMMER is limited to one.  
:cuefn      - Optional cue-function, defaults to *default-cue-function*
:shuffle    - Optional shuffle-function, defaults to #'NO-SHUFFLE
:stream     - Result stream has same usage as Lisp format function.
              t   - (default) prints to terminal.
              nil - returns string.

Returns a string suitable for creating a STRUMMER equivalent to the qball. 
Some manual editing may be necessary")


(setf (documentation 'xball->strummer 'function)
      "Translates XBALL to readable string representing an equivalent STRUMMER.

xball       - The source XBALL
:name       - Optional strummer name, defaults to qball's name.
:instrument - Optional instrument name, defaults to xball's instrument.
:shuffle    - Optional shuffle-function, defaults to #'NO-SHUFFLE
:stream     - Result stream has same usage as Lisp format function.
              t   - (default) prints to terminal.
              nil - returns string.			 
			 
Returns a string suitable for creating a STRUMMER equivalent to the xball. 
Some manual editing may be necessary.")
