;;;; CYCO composition bar48
;;;;
;;;; Derfines cueing function for use with 48-ticks per quarter note
;;;;
;;;;                 1         2         3         4       
;;;;       012345678901234567890123456789012345678901234567
;;;; Q     Q     |     |     |     |     |     |     |         0
;;;; E     E.....|.....|.....|.....E.....|.....|.....|.....    0 24
;;;; S     S.....|.....S.....|.....S.....|.....S.....|.....    0 12 24 36
;;;; T     T.....T.....T.....T.....T.....T.....T.....T.....    0  6 12 18 24 30 36 42
;;;; X     X..X..X..X..X..X..X..X..X..X..X..X..X..X..X..X..    0  3  6  9 12 15 18 21 24 27 30 33 36 39 42 45
;;;;       1           e           +           a
;;;;       1     e-    e     e+    +     a-    a     a+
;;;;       1     e-    e     +-    +     ++    a     a+
;;;;
;;;;                 1         2         3         4       
;;;;       0123456789012345678901234567890123456789012345678
;;;; ET    E       |       E       |       E       |           0 16 32
;;;; ST    S.......S.......S.......S.......S.......S.......    0  8 16 24 32 40
;;;; TT    T...T...T...T...T...T...T...T...T...T...T...T...    0  4  8 12 16 20 24 28 32 36 40 44
;;;;       t1              t2              t3
;;;;       st1     st2     st3     st4     st5     st6

;;;; symbolic  numeric
;;;; (1 1  1) (1 1  0)  (1 1 1) -> down beat
;;;; (1 1  e) (1 1 12)          -> 2nd 1/16
;;;; (1 1  +) (1 1 24)          -> up beat
;;;; (1 1  a) (1 1 36)          -> 3rd 1/16



(in-package :cyco)

(set-default-time-signature :tempo 60 :unit 'q :bars 4 :beats 4 :subbeats 48 :ticks 480)


(param *bar48-symbols* (make-hash-table :size 18))
(setf (gethash 'e- *bar48-symbols*)  6
      (gethash 'e  *bar48-symbols*) 12
      (gethash 'e+ *bar48-symbols*) 18
      (gethash '+- *bar48-symbols*) 18
      (gethash '+  *bar48-symbols*) 24
      (gethash 'a- *bar48-symbols*) 30
      (gethash '++ *bar48-symbols*) 30
      (gethash 'a  *bar48-symbols*) 36
      (gethash 'a+ *bar48-symbols*) 42
      (gethash 'tt1  *bar48-symbols*)  0  ;; thirty-second tripplets
      (gethash 'tt2  *bar48-symbols*)  4
      (gethash 'tt3  *bar48-symbols*)  8
      (gethash 'tt4  *bar48-symbols*) 12
      (gethash 'tt5  *bar48-symbols*) 16
      (gethash 'tt6  *bar48-symbols*) 20
      (gethash 'tt7  *bar48-symbols*) 24
      (gethash 'tt8  *bar48-symbols*) 28
      (gethash 'tt9  *bar48-symbols*) 32
      (gethash 'tt10 *bar48-symbols*) 36
      (gethash 'tt11 *bar48-symbols*) 40
      (gethash 'tt12 *bar48-symbols*) 44
      (gethash 'st1  *bar48-symbols*)  0  ;; sixteenth trippets
      (gethash 'st1  *bar48-symbols*)  8
      (gethash 'st1  *bar48-symbols*) 16
      (gethash 'st1  *bar48-symbols*) 24
      (gethash 'st1  *bar48-symbols*) 32
      (gethash 'st1  *bar48-symbols*) 40
      (gethash 'et1  *bar48-symbols*)  0  ;; eigth tripplets
      (gethash 'et1  *bar48-symbols*) 16
      (gethash 'et1  *bar48-symbols*) 32)
      


(labels ((warnfn (args)
		 (cyco-cue-warning 'BAR48 args)
		 0.0)

	 (bar-value (tsig cue token)
		    (or (and (integerp token)(plusp token)(<= token (bars tsig))
			     (* (bar-duration tsig)(1- token)))
			(warnfn cue)))

	 (beat-value (tsig cue token)
		     (or (and (integerp token)(plusp token)(<= token (beats tsig))
			      (* (beat-duration tsig)(1- token)))
			 (warnfn cue)))

	 ;; 0 1 2 ... 47
	 (integer-subbeat (tsig cue token)
			  (cond ((minusp token)
				 (warnfn cue))
				((or (= token 1)(= token 0))  ;; 1 treated as zero as special case
				 0.0)
				((< token (subbeats tsig))
				 (* (subbeat-duration tsig) token))
				(t (warnfn cue))))

	 (symbol-subbeat (tsig cue token)
			 (let ((count (gethash token *bar48-symbols*)))
			   (or (and count (* count (subbeat-duration tsig)))
			       (warnfn cue))))
			 
				 
	 
	 (subbeat-value (tsig cue token)
			(cond ((integerp token)
			       (integer-subbeat tsig cue token))
			      ((symbolp token)
			       (symbol-subbeat tsig cue token))
			      (t (warnfn cue))))


	 (tick-value (tsig cue token)
		     (cond ((and (integerp token)(< (abs token)(subbeats tsig)))
			    (* token (subbeat-duration tsig)))
			   ((and (floatp token)(< (abs token) 1.0))
			    (* token (beat-duration tsig)))
			   (t (warnfn cue))))
			       
	 
	 ) ;; end labels assignments

	(defun bar48 (tsig cue)
	  (let* ((v (->vector (fill-list (->list cue) '(1 1 0 0))))
		 (br (bar-value tsig cue (aref v 0)))
		 (bt (beat-value tsig cue (aref v 1)))
		 (sb (subbeat-value tsig cue (aref v 2)))
		 (tk (tick-value tsig cue (aref v 3))))
	    (float (+ br bt sb tk))))
		 
		    

	) ;; end labels

