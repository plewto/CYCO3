;;;; CYCO plugin cue48
;;;;
;;;; Changes the following defaults for new projects.
;;;;
;;;;  * Sets default subbeats-per-beat to 48
;;;;  * Changes default cueing function to cue48, defined below
;;;;
;;;;
;;;; Defines cueing function for use with 48-ticks per quarter note
;;;;
;;;;       0         1         2         3         4       
;;;;       012345678901234567890123456789012345678901234567
;;;; Q     Q     |     |     |     |     |     |     |      
;;;; E     E.....|.....|.....|.....E.....|.....|.....|..... 
;;;; S     S.....|.....S.....|.....S.....|.....S.....|..... 
;;;; T     T.....T.....T.....T.....T.....T.....T.....T..... 
;;;; X     X..X..X..X..X..X..X..X..X..X..X..X..X..X..X..X.. 
;;;;       1           e           +           a
;;;;       1     e-    e     e+    +     a-    a     a+
;;;;       1     e-    e     +-    +     ++    a     a+
;;;;
;;;;       0         1         2         3         4       
;;;;       0123456789012345678901234567890123456789012345678
;;;; ET    E       |       E       |       E       |          
;;;; ST    S.......S.......S.......S.......S.......S.......   
;;;; TT    T...T...T...T...T...T...T...T...T...T...T...T...   
;;;;       t1              t2              t3
;;;;       st1     st2     st3     st4     st5     st6


(in-package :cyco)

(set-default-time-signature :tempo 60 :unit 'q :bars 4 :beats 4 :subbeats 48 :ticks 480)

(param *cue48-symbols* (make-hash-table :size 18))

(defun def-cue48 (symbol ticks)
  "Sets a CUE48 subbeat symbol assignment.
symbol - the symbol.
ticks  - number of subbeat ticks associated with the symbol."
  (setf (gethash symbol *cue48-symbols*) ticks))

(def-cue48 'e-    6) ;; basic sixteenth & thirty-second notes.
(def-cue48 'e    12)
(def-cue48 'e+   18)
(def-cue48 '+-   18)
(def-cue48 '+    24)
(def-cue48 'a-   30)
(def-cue48 '++   30)
(def-cue48 'a    36)
(def-cue48 'a+   42)
(def-cue48 'tt1   0) ;; thirty-second triplets
(def-cue48 'tt2   4)
(def-cue48 'tt3   8)
(def-cue48 'tt4  12)
(def-cue48 'tt5  16)
(def-cue48 'tt6  20)
(def-cue48 'tt7  24)
(def-cue48 'tt8  28)
(def-cue48 'tt9  32)
(def-cue48 'tt10 36)
(def-cue48 'tt11 40)
(def-cue48 'tt12 44)
(def-cue48 'st1   0) ;; sixteenth tippets
(def-cue48 'st2   8)
(def-cue48 'st3  16)
(def-cue48 'st4  24)
(def-cue48 'st5  32)
(def-cue48 'st6  40)
(def-cue48 'et1   0) ;; eighth triplets
(def-cue48 'et2  16)
(def-cue48 'et3  32)

(labels ((errorfn (args)
		 (cyco-cue-error 'CUE48 args)
		 0.0)

	 (bar-value (tsig cue token)
		    (or (and (integerp token)(plusp token)(<= token (bars tsig))
			     (* (bar-duration tsig)(1- token)))
			(errorfn cue)))

	 (beat-value (tsig cue token)
		     (or (and (integerp token)(plusp token)(<= token (beats tsig))
			      (* (beat-duration tsig)(1- token)))
			 (errorfn cue)))

	 ;; 0 1 2 ... 47
	 (integer-subbeat (tsig cue token)
			  (cond ((minusp token)
				 (errorfn cue))
				((or (= token 1)(= token 0))  ;; 1 treated as zero as special case
				 0.0)
				((< token (subbeats tsig))
				 (* (subbeat-duration tsig) token))
				(t (errorfn cue))))

	 (symbol-subbeat (tsig cue token)
			 (let ((count (gethash token *cue48-symbols*)))
			   (or (and count (* count (subbeat-duration tsig)))
			       (errorfn cue))))
	 
	 (subbeat-value (tsig cue token)
			(cond ((integerp token)
			       (integer-subbeat tsig cue token))
			      ((symbolp token)
			       (symbol-subbeat tsig cue token))
			      (t (errorfn cue))))


	 (tick-value (tsig cue token)
		     (cond ((and (integerp token)(< (abs token)(subbeats tsig)))
			    (* token (tick-duration tsig)))
			   ((and (floatp token)(< (abs token) 1.0))
			    (* token (beat-duration tsig)))
			   (t (errorfn cue)))) )

	;; cue list (bar beat subbeat tick)
	;;
	(defun cue48 (tsig cue)
	  (let* ((v (->vector (fill-list (->list cue) '(1 1 0 0))))
		 (br (bar-value tsig cue (aref v 0)))
		 (bt (beat-value tsig cue (aref v 1)))
		 (sb (subbeat-value tsig cue (aref v 2)))
		 (tk (tick-value tsig cue (aref v 3))))
	    (float (+ br bt sb tk))))
		 
	(setf *default-cue-function* #'cue48)
	(set-default-time-signature :subbeats 48) )

(setf (documentation 'cue48 'function)
      "CUE48 is a cuing function explicitly for use when a time-signature's
      subbeats-per-beat is 48.   Call (?CUE48) for details.")

(defun ?cue48 ()
  (let ((text "
  The CUE48 plugin changes new project defaults to use 
  48 subbeats-per-beat and the CUE48 cuing function.

  (cue48 time-signature cue)

  where cue is a list of form  (BAR BEAT SUBBEAT TICKS).
  All values are optional with reasonable defaults. 

  BAR     - the bar number 1, 2, ... (bars time-signature)   
  BEAT    - the beat within bar 1, 2, ... (beats time-signature)
  SUBBEAT - the subbeat within the beat.
            The subbeat may be either an integer (0, 1, 2, ... 47)
            or a symbol listed below. 

            NOTE: For notation consistency a subbeat of 1 is 
                  converted to 0. There is no other automatic 
                  voodoo.

            eighth-notes   numeric    0  24
                           symbolic   1  +

            sixteenth      numeric    0  12 24 36
                           symbolic   1  e  +  a

            thirty-second  numeric    0  6  12 18 24 30 36 42
                           symbolic   1  e- e  e- +  a- a  a+
                                              +-    ++
             
            eighth         numeric   0   16  32
            triplet        symbolic  et1 et2 et3
            
            sixteenth      numeric   0    8   16  24  32  40
            triplet        symbolic  st1  st2 st3 st4 st5 st6

            thirty-second  numeric  0   4   8   12  16  20  24  28  32  36   40   44
            triplet        symbolic  tt1 tt2 tt3 tt4 tt5 tt6 tt7 tt7 tt9 tt10 tt11 tt12
 
 
  TICKS   - number of clock ticks added/removed from time.  
            The ticks value may either be an integer ... -2 -1 0 +1 +2 ...
            or a float between -1.0 <= ticks <= +1.0

           For an integer n, n ticks duration is added.
           For a float f, the value f*beat is used.

  The DEF-CUE48 function may be used to define a custom CUE48 symbol."))
    (format t "~A~%" text)
    (maphash #'(lambda (key value)
		 (format t "[~4A] -> ~3D~%" key value))
	     *cue48-symbols*)))
	     

(defun cue48-prompt-function (s)
  (let* ((dir (final (partition-path (cwd))))
	 (sec (and (project-p *project*)(property *project* :current-section)))
	 (sname (or (and sec (name sec)) "")))
    (format s "~%CYCO(Q48) ~A ~A: " dir sname)))

(if (equalp (lisp-implementation-type) "SBCL")
    (setf sb-int:*REPL-PROMPT-FUN* #'cue48-prompt-function))
