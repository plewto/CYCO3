;;;; CYCO inspection/overview
;;;;   
;;;; Defines functions for inspecting objects.
;;;;

(in-package :cyco)

(defmacro ?d (sym)
  "Convenience macro for describe.
Do not quote the argument, use (?d foo)  not (?d 'foo)"
  `(describe ',sym))

(defmacro ?a (sym &optional (package :cyco))
  "Convenience macro for apropos.
Do not quote the argument.  Use (?a foo)  not (?a 'foo)"
  `(apropos ',sym ,package))

(defun ?kmap (instrument)
  "Displays instrument's keymap documentation."
  (format t ";; ~A~%" (name instrument))
  (funcall (keynumber-map instrument) :doc))

(defun ?pmap (instrument)
  "Displays instrument's program-map documentation"
  (format t ";; ~A~%" (name instrument))
  (funcall (program-map instrument) 0 :program :doc))

(let ((ary (->vector (copies 16 '()))))
  (labels ((walk (inst)
		 (let* ((ci (channel-index inst))
			(ilst (aref ary ci)))
		   (push (name inst) ilst)
		   (setf (aref ary ci) ilst)
		   (dolist (c (children inst))
		     (walk c)))))
    (defun ?channels ()
      "Displays list of instruments on each MIDI channel."
      (setf ary (->vector (copies 16 '())))
      (walk +root-instrument+)
      (dotimes (i (length ary))
	(let ((chan (1+ i))
	      (ilst (aref ary i)))
	  (dolist (iname ilst)
	    (format t "[~2D] ~A~%" chan iname)))))))

(defun ?o (&optional (root +root-instrument+))
  "Displays orchestra tree."
  (print-tree root))

(defun ?p ()
  "Displays project structure."
  (if *project*
      (print-tree *project*)
    (format t "There is no current project, *PROJECT* is NIL~%")))

(defun ?? ()
  "Displays list of available inspection functions."
  (format t "(? object)      Interactive object inspection~%.")
  (format t "(?a symbol)     Shortcut for (apropos symbol)  do not quote symbol~%")
  (format t "(?d symbol)     Shortcut for (describe symbol) do not quote symbol~%")
  (format t "(?o)            Displays orchestra tree.~%")
  (format t "(?p)            Displays current project structure.~%")
  (format t "(?chords)       Display chord table, takes optional argument.~%")
  (format t "(?channels)     Display MIDI channel usage.~%")
  (format t "(?controllers)  Display assigned MIDI controllers.~%")
  (format t "(?kmap inst)  Displays instrument's keynumber-map documentation.~%")
  (format t "(?pmap inst)  Displays instrument's program map documentation.~%"))

