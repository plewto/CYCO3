;;;; CYCO
;;;;

(defpackage :cyco
  (:use common-lisp))

(in-package :cyco)

(defun dismiss (&rest args)
  "Informs SBCL compiler that arguments may be ignored."
  args)

(defun set-cyco-prompt ())

(defmacro constant (name value &optional (docstring ""))
  "An alias for defconstant"
  `(if (not (boundp ',name))
       (defconstant ,name ,value ,docstring)))

(defmacro param (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro global (name &optional (value nil)(docstring ""))
  "An alias for defparameter."
  `(defparameter ,name ,value ,docstring))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun bool (obj)
  "Returns canonical boolean t or nil."
  (if obj t nil))

(defmacro constant-function (name value)
  `(defun ,name (&rest args)
     (dismiss args)
     (format nil "Returns constant ~A" ,value)
     ,value))

(constant-function true t)
(constant-function false nil)



(let* ((current "")
       (manifest '("src/constants"
		   "src/globals"
		   "src/generics"
		   "src/string-utilities"
		   "src/notifications"
		   "src/seq-utilities"
		   "src/math-utilities"
		   "src/paths"
		   "src/keynumbers"
		   "src/dynamics"
		   "src/metrics"
		   "src/patterns/pattern-generics"
		   "src/patterns/pattern"
		   "src/patterns/line"
		   "src/patterns/cycle"
		   "src/patterns/coin"
		   "src/patterns/bag"
		   "src/patterns/dice"
		   "src/patterns/instrument-layer"
		   "src/node"
		   "src/midi/midi"
		   "src/midi/midi-util"
		   "src/midi/event"
		   "src/midi/syscommon"
		   "src/midi/meta"
		   "src/midi/smf-header"
		   "src/midi/smf-track"
		   "src/midi/smf"
		   "src/orchestra/channel-assignments"
		   "src/orchestra/program-map"
		   "src/orchestra/keynumber-map"
		   "src/orchestra/dynamic-map"
		   "src/orchestra/articulation-map"
		   "src/orchestra/instrument"
		   "src/chords/chord-model"
		   "src/chords/chord-table"
		   "src/chords/default-chords"
		   "src/composition/time-signature"
		   "src/composition/cueing-functions"
		   "src/composition/project"
		   "src/composition/group"
		   "src/composition/section"
		   "src/composition/parts/part"
		   "src/composition/parts/raw-part"
		   "src/composition/parts/epart-state"
		   "src/composition/parts/epart"
		   "src/composition/parts/epart-render"
		   "src/composition/parts/cpart"
		   "src/composition/parts/cpart-render"
		   "src/composition/parts/qball"
		   "src/composition/parts/programs"
		   "src/composition/parts/metronome"
		   "src/composition/countin"
		   "src/composition/endpad"
		   "src/inspection"
		   "src/local-config"
		   )))

 
  (defun ld (filename &key (verbose t)(print nil))
    (let ((temp *load-print*))
      (setf current filename)
      (if verbose
	  (format t "~A~%" filename))
      (setf *load-print* print)
      (load filename)
      (setf *load-print* temp)
      current))

  (defun rl ()
    "Reloads most-recently loaded CYCO source file."
    (ld current))

  (defun build-cyco (&key (verbose t)(print nil))
    "Reloads all CYCO source files."
    (dolist (file manifest)
      (ld file :verbose verbose :print print))) )
  
(build-cyco)

;; #| *** ISSUE: Uncomment in production 
(let ((lit (lisp-implementation-type)))
  (cond
   ;;((equalp lit "Armed Bear Common Lisp")
    ;;(ld "src/cyco-abcl"))
   ((equalp lit "SBCL")
    (ld "src/sbcl"))
   (t
    (format t "WARNING: ~A is an unsupoted Lisp.  Using  SBCL instead.~%" lit)
    (ld "src/sbcl"))))
;; *** |#

(in-package :common-lisp-user)

(defun cyco ()
  (in-package :cyco)
  (cyco::set-cyco-prompt)
  (cyco::cyco-banner)
  nil)

(in-package :cyco)

(defun cyco ()
  (cyco-banner))
