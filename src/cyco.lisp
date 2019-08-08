;;;; CYCO cyco-loaders
;;;;

(defpackage :cyco
  (:use :cl))

(in-package :cyco)


(defun dismiss (&rest args)
  "Informs SBCL compiler that arguments may be ignored."
  args)

(defun set-cyco-prompt ())

(defmacro constant (name value &optional (docstring ""))
  "An alias for defconstant"
  `(if (not (boundp ',name))
       (defconstant ,name ,value ,docstring)))

(constant +build-time+ (get-universal-time))

;; (defun build-time ()
;;   "Returns association list of CYCO build time."
;;   (multiple-value-bind
;;       (second minute hour date month year day-of-week dst-p tz)
;;       (decode-universal-time (get-universal-time))
;;     (dismiss second day-of-week dst-p tz)
;;     (list (cons :year year)
;; 	  (cons :month month)
;; 	  (cons :date date)
;; 	  (cons :hour hour)
;; 	  (cons :minute minute))))
	  
	  

;; (defun format-build-time ()
;;   "Formats CYCO build time"
;;   (let ((time (build-time)))
;;     (format nil "Build ~A/~A/~A  ~A:~A"
;; 	    (cdr (assoc :year time))
;; 	    (cdr (assoc :month time))
;; 	    (cdr (assoc :date time))
;; 	    (cdr (assoc :hour time))
;; 	    (cdr (assoc :minute time)))))

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

(defun bool (object)
  "Returns canonical boolean t or nil."
  (if object t nil))

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
(defun exit (&optional code)
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
      (error 'not-implemented :proc (list 'exit code))) 

(let* ((current "")
       (verbose-flag t)
       (print-flag nil)
       ;; List of CYCO source files
       ;; For testing manifest may include following keywords
       ;;     :break - Do not load remaining files in manifest.
       ;;     :exit  - Exit lisp.
       ;;     :print-on  - print load on.
       ;;     :print-off - print load off.
       ;;     :verbose - Display file names as they are loaded.
       ;;     :quite   - Opposite of :verbose
       (manifest '(:verbose
		   "src/constants"
		   "src/globals"
		   "src/generics"
		   "src/util/string-utilities"
		   "src/util/banner"
		   "src/util/notifications"
		   "src/util/seq-utilities"
		   "src/util/math-utilities"
		   "src/util/paths"
		   "src/keynumbers"
		   "src/keytables"
		   "src/dynamics"
		   "src/metrics"
		   "src/patterns/pattern"
		   "src/patterns/line"
		   "src/patterns/cycle"
		   "src/patterns/coin"
		   "src/patterns/bag"
		   "src/patterns/dice"
		   "src/patterns/wrapper"
		   "src/patterns/slewpat"
		   "src/patterns/walker"
		   "src/patterns/markov"
		   ;; "src/patterns/shift-register" ; converted to plugin
		   "src/patterns/merger"
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
		   "src/composition/shuffle"
		   "src/composition/cueing-functions"
		   "src/composition/project/framework"
		   "src/composition/project/persistence"
		   "src/composition/project/section-render-mode"
		   "src/composition/project/project"
		   "src/composition/project/project-render"
		   "src/composition/project/loader"
		   "src/composition/group"
		   "src/composition/sections/section"
		   "src/composition/parts/part"
		   "src/composition/parts/raw-part"

		   "src/composition/parts/strummer/strummer-docs"
		   "src/composition/parts/strummer/strummer-state"
          	   "src/composition/parts/strummer/strummer"
		   "src/composition/parts/strummer/strummer-render"

		   "src/composition/parts/simple/header"

		  


		   "src/composition/parts/controllers"
		   "src/composition/parts/controllers-render"
		   "src/composition/parts/qball"

		   
		   
		   "src/composition/parts/ghost"
		   "src/composition/parts/programs"
		   "src/composition/parts/metronome"
		   "src/composition/sections/countin"
		   "src/composition/sections/endpad"
		   "src/util/inspection"
		   "src/plugins"
		   "src/cyco-exports"
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
	    ((eq file :exit)
	     (format t "BUILD-CYCO exited with :EXIT command~%")
	     (exit))
	    ((eq file :verbose)
	     (setf verbose-flag t))
	    ((eq file :quite)
	     (setf verbose-flag nil))
	    ((eq file :print-on)
	     (setf print-flag t))
	    ((eq file :print-off)
	     (setf print-flag nil))
	    (t (ld file :verbose verbose :print print))))))

  
(build-cyco)

;; Customize for specific Lisp implementations
(let ((lit (lisp-implementation-type)))
  (cond
   ((equalp lit "SBCL")
    (ld "src/sbcl"))
   (t
    (format t "WARNING: Using non-tested Lisp: ~A ~%" lit))))

(defun ?version ()
  "Displays current version (major minor revision))"
  (format t "Version: ~A~%" +cyco-version+))

(defun version (major &optional minor)
  "Enforces version.
If either major dose not match current major version number, 
or minor is specified and it is less then current minor version number,
a warning message is displayed and CYCO terminates."
  (let ((mj (eq major (car +cyco-version+)))
	(mn  (or (not minor)
		 (>= minor (second +cyco-version+)))))
    (if (not (and mj mn))
	(progn 
	  (cyco-warning "CYCO version mismatch"
			(sformat "Current version   : ~A" +cyco-version+)
			(sformat "Specified version : (~A ~A NIL)" major minor))
	  (exit)))))


(in-package :common-lisp-user)

(defun cyco ()
  "Switch to CYCO namespace"
  (in-package :cyco)
  (cyco::set-cyco-prompt)
  (cyco::cyco-banner)
  ;;(format t "~A~%" (cyco::format-build-time))
  nil)

(in-package :cyco)

(plugin 'ion)  ;; ISSUE FOR personal use ONLY, REMOVE IN PRODUCTION CODE
(defun lsr ()(plugin 'shift-register :t))  ;; ISSUE FOR TESTING ONLY


(defun cyco ()
  (cyco-banner)
  (set-cyco-prompt)
  nil)
