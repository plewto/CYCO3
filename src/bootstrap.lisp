(in-package :cyco)

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
       (manifest '(:quite
		   "src/constants"
		   "src/generics"
		   "src/util/string-utilities"
		   "src/util/banner"
		   "src/util/notifications"
		   "src/util/seq-utilities"
		   "src/util/math-utilities"
		   "src/util/paths"
		   "src/util/cwd"
		   "src/util/dirlist"
		   "src/util/table-wrapper"
		   "src/keynumbers"
		   "src/keytables"
		   "src/dynamics"
		   "src/metrics"
		   "src/controller-number-table"
		   "src/patterns/pattern"
		   "src/patterns/line"
		   "src/patterns/cycle"
		   "src/patterns/coin"
		   "src/patterns/bag"
		   "src/patterns/dice"
		   "src/patterns/wrapper"
		   "src/patterns/walker"
		   "src/patterns/instrument-layer"
		   "src/patterns/switch"
		   "src/patterns/slowglass"
		   "src/patterns/sample"
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

		   "src/generators/generator"
		   "src/generators/counter"
		   "src/generators/ramp"
		   "src/generators/envelope"
		   "src/generators/waveshapes"
		   "src/generators/shift-register"
		   "src/generators/hailstone"
		   "src/generators/recaman"
		   "src/generators/bones"
		   "src/generators/logistic"
		   "src/generators/alloy"
		   "src/generators/euclid"
		   
		   "src/parts/parts-header"
		   "src/parts/expect"
		   "src/parts/part"
		   "src/parts/raw-part"
		   "src/parts/qball-docs"
		   "src/parts/qball"
		   "src/parts/qball-midi-render"
		   "src/parts/metronome"
		   "src/parts/programs"
		   "src/parts/strummer-docs"
		   "src/parts/strummer-state"
		   "src/parts/strummer"
		   "src/parts/strummer-midi-render"
		   "src/parts/controllers-docs"
		   "src/parts/controllers-state"
		   "src/parts/controllers"
		   "src/parts/controllers-render"
		   "src/parts/mixer"
		   "src/parts/transformer-docs"
		   "src/parts/transformer"
		   "src/parts/text"
		   "src/parts/sysex-docs"
		   "src/parts/sysex"
		   "src/parts/export"
		   
		   "src/composition/sections/preroll"
		   "src/composition/sections/fin"
		   "src/util/inspection"
		   "src/plugins"
		   "src/local-config"
		   "src/exports")))

  (labels  ((load-cyco-source (filename &key (verbose nil)(print nil))
		"Loads CYCO source file."
		(let ((temp *load-print*))
		  (setf current filename)
		  (if (or verbose-flag verbose)
		      (format t "~A~%" filename))
		  (setf *load-print* (or print-flag print))
		  (load filename)
		  (setf *load-print* temp)
		  current))

	    ;; Customize for specific Lisp implementations
	    (customize ()
		       (let ((lit (lisp-implementation-type)))
			 (cond
			  ((equalp lit "SBCL")
			   (load-cyco-source "src/sbcl"))
			  ((member lit '("Armed Bear Common Lisp") :test #'string=)
			   nil)
			  (t
			   (format t "WARNING: Using non-tested Lisp: ~A ~%" lit))))))
  
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
	      (t (load-cyco-source file :verbose verbose :print print))))
      (customize)) ))

(build-cyco :verbose nil)


