;;;; CYCO cyco.lisp
;;;;
;;;; Main entry point to build CYCO.
;;;;

(defpackage :cyco
  (:use :cl))

(in-package :cyco)

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

(defun bool (object)
  "Returns canonical boolean t or nil."
  (if object t nil))

(defmacro constant-function (name value)
  `(defun ,name (&rest args)
     (declare (ignore args))
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
		   "src/util/table-wrapper"
		   "src/keynumbers"
		   "src/keytables"
		   "src/dynamics"
		   "src/metrics"
		   "src/controller-number-table"
		   "src/patterns/pattern"
		   "src/patterns/line"
		   "src/patterns/cycle"
		   "src/patterns/pseudo-patterns"  ;; DEPRECIATED
		   "src/patterns/coin"
		   "src/patterns/bag"
		   "src/patterns/dice"
		   "src/patterns/wrapper"
		   "src/patterns/walker"
		   "src/patterns/instrument-layer"
		   "src/generators/generator"
		   "src/generators/envelope"
		   "src/generators/lfo"
		   "src/generators/shift-register"
		   "src/generators/hailstone"
		   "src/generators/recaman"
		   "src/node"
		   "src/midi/midi-message"
		   "src/midi/midi-util"
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
		   "src/parts/parts-header"
		   "src/composition/sections/preroll"
		   "src/composition/sections/fin"
		   "src/util/inspection"
		   "src/plugins"
		   "src/local-config"
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
   ((member lit '("SBCL" "CLISP") :test #'string=)
    nil)
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
  nil)

(in-package :cyco)

(defun cyco ()
  (cyco-banner)
  (set-cyco-prompt)
  nil)


;;; TODO for developement only, remove
;;;
(defun b nil (build-cyco))
