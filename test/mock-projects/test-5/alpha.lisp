;;;; test-5 alpha   basic events.
;;;;

(section alpha)

(format t "ALPHA non-key events~%")
(strummer alpha-no-key piano1
	 :events '((:time 0.000 :reset)
		   (:time 0.001 :bend 0.0)
		   (:time 0.002 :bend 0.1)
		   (:time 0.003 :cc 4 1.0)
		   (:time 1.000 :program 13)
		   (:time 1.001 :program default)
		   (:time 2.000 :bank 10 13)
		   (:time 3.000 :bank default default)))
(dump-events alpha-no-key)
(terpri)	 


(format t "ALPHA Basic note events (no chord)~%")
(strummer alpha-basic-key piano1
	  :events '((:chord [solo] :dur 1.0 :amp mf)
		    (:time 0.000 :key c5)))
(dump-events alpha-basic-key)
(terpri)

(format t "ALPHA dynamic blur~%")
(strummer alpha-dynamic-blur piano1
	  :events '((:chord [solo] :dur 1.0 :amp 0.5 :amp-blur 0.00)
		    (:time 0.000 :key 60)
		    (:time 0.001 :key 61)
		    (:time 0.002 :key 62)
		    (:time 0.003 :key 63)

		    (:time 1.010 :key 60 :amp-blur 0.5)
		    (:time 1.011 :key 61)
		    (:time 1.012 :key 62)
		    (:time 1.013 :key 63)))
(dump-events alpha-dynamic-blur :filter #'midi-note-off-p)
(terpri)


(format t "ALPHA dynamic Pattern~%")
(strummer alpha-dynamic-pattern piano1
	  :events '((:chord [solo] :dur 1.0 :amp (0.1 0.5 0.9) :amp-blur 0.00)
		    (:time 0.000 :key 60)
		    (:time 0.001 :key 61)
		    (:time 0.002 :key 62)
		    (:time 0.003 :key 63)
		    (:time 1.010 :key 60)
		    (:time 1.011 :key 61)
		    (:time 1.012 :key 62)
		    (:time 1.013 :key 63)))
(dump-events alpha-dynamic-pattern :filter #'midi-note-off-p)
(terpri)

(format t "ALPHA dynamic crescendo/decrescendo~%")
(strummer alpha-crescendo piano1
	  :events '((:chord [solo] :dur 1.0 :cres 0.1 0.9 4 :amp-blur 0.00)
		    (:time 0.000 :key 60)
		    (:time 0.001 :key 61)
		    (:time 0.002 :key 62)
		    (:time 0.003 :key 63)
		    (:time 0.004 :key 60)
		    (:time 0.005 :key 61)
		    (:time 0.006 :key 62)
		    (:time 0.007 :key 63)

		    (:time 1.000 :key 60 :cres 0.9 0.1 6)
		    (:time 1.001 :key 61)
		    (:time 1.002 :key 62)
		    (:time 1.003 :key 63)
		    (:time 1.004 :key 60)
		    (:time 1.005 :key 61)
		    (:time 1.006 :key 62)
		    (:time 1.007 :key 63)))
(dump-events alpha-crescendo :filter #'midi-note-off-p)
(terpri)

