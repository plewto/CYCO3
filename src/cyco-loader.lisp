;;;; CYCO cyco-loaders
;;;;


;; (defpackage :cyco
;;   (:use :cl ))

;; (in-package :cyco)


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

;; Taken from Rob Warnock's post "How to programmatically exit?"
;; https://groups.google.com/forum/#!msg/comp.lang.lisp/kpTqyLQ-HaU/ms2NYHmlyZQJ
;;
(defun quit (&optional code)
      ;; This group from "clocc-port/ext.lisp"
      #+allegro (excl:exit code)
      #+clisp (#+lisp=cl ext:quit #-lisp=cl lisp:quit code)
      #+cmu (ext:quit code)
      #+cormanlisp (win32:exitprocess code)
      #+gcl (lisp:bye code)                     ; XXX Or is it LISP::QUIT?
      #+lispworks (lw:quit :status code)
      #+lucid (lcl:quit code)
      #+sbcl (sb-ext:exit :code code)
      ;; This group from Maxima
      #+kcl (lisp::bye)                         ; XXX Does this take an arg?
      #+scl (ext:quit code)                     ; XXX Pretty sure this *does*.
      #+(or openmcl mcl) (ccl::quit)
      #+abcl (cl-user::quit)
      #+ecl (si:quit)
      ;; This group from <hebi...@math.uni.wroc.pl>
      #+poplog (poplog::bye)                    ; XXX Does this take an arg?
      #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl
            kcl scl openmcl mcl abcl ecl)
      (error 'not-implemented :proc (list 'quit code))) 


(let* ((current "")
       (verbose-flag t)
       (print-flag nil)
       ;; List of CYCO source files
       ;;     :break - Do not load remeining files in manifest
       ;;     :quit  - Exit lisp
       ;;     :print-on  - print load on
       ;;     :print-off - print load off
       ;;     :verbose - Display file names as loaded
       ;;     :quite   - Oposite of :verbose
       (manifest '(:QUITE
		   "src/constants"
		   "src/globals"
		   "src/api"
		   "src/util/string-utilities"
		   "src/util/banner"
		   "src/util/notifications"
		   "src/util/seq-utilities"
		   "src/util/math-utilities"
		   "src/util/paths"
		   "src/keynumbers"
		   "src/dynamics"
		   "src/metrics"
		   "src/patterns/pattern"
		   "src/patterns/line"
		   "src/patterns/cycle"
		   "src/patterns/coin"
		   "src/patterns/bag"
		   "src/patterns/dice"
		   "src/patterns/instrument-layer"
		   "src/node"
		   "src/midi/midi-util"
		   "src/midi/midi-message"
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
		   "src/composition/parts/epart-docs"
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
		   "src/util/inspection"
		   "src/local-config"
		   )))
 
  (defun ld (filename &key (verbose t)(print nil))
    "Loads CYCO source file."
    (let ((temp *load-print*))
      (setf current filename)
      (if (or verbose-flag verbose)
	  (format t "~A~%" filename))
      (setf *load-print* (or print-flag print))
      (load filename)
      (setf *load-print* temp)
      current))

  (defun rl ()
    "Reloads most-recently loaded CYCO source file."
    (ld current))

  (defun build-cyco (&key verbose print)
    "Reloads all CYCO source files."
    (format t "Building CYCO...~%")
    (dolist (file manifest)
      (cond ((eq file :break)
	     (format t "BUILD-CYCO exited with :BREAK command~%")
	     (return-from build-cyco))
	    ((eq file :quit)
	     (format t "BUILD-CYCO exited with :QUIT command~%")
	     (quit))
	    ((eq file :verbose)
	     (setf verbose-flag t))
	    ((eq file :quite)
	     (setf verbose-flag nil))
	    ((eq file :print-on)
	     (setf print-flag t))
	    ((eq file :print-off)
	     (setf print-flag nil))
	    (t (ld file :verbose verbose :print print))))) )
  
(build-cyco)

;; Customize for specific Lisp implementations
(let ((lit (lisp-implementation-type)))
  (cond
   ((equalp lit "SBCL")
    (ld "src/sbcl"))
   (t
    (format t "WARNING: Using non-tested Lisp: ~A ~%" lit))))

(in-package :common-lisp-user)

(defun cyco ()
  "Switch to CYCO namespace"
  (in-package :cyco)
  (cyco::set-cyco-prompt)
  (cyco::cyco-banner)
  nil)

(in-package :cyco)


(defun cyco ()
  (cyco-banner))
