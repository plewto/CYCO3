;;;; test-5 strum & chords


(terpri)
(section strum)


(format t "strum-1  major chord  no struming~%")
(strummer strum-1 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :strum* 1.0 :amp* 1.0 :amp-blur 0.0 :direction down)
		    (:time 0.000 :key 60 :chord [maj]  :inversion 0 :octave 0)))

(dump-events strum-1 :filter #'midi-note-off-p)
(terpri)


(format t "strum-2  major chord  :inversion 0 :octave 1~%")
(strummer strum-2 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :strum* 1.0 :amp* 1.0 :amp-blur 0.0 :direction down)
		    (:time 0.000 :key 60 :chord [maj]  :inversion 0 :octave 1)))
(dump-events strum-2 :filter #'midi-note-off-p)
(terpri)



(format t "strum-3  major chord  :inversion 1&2 :octave 1~%")
(strummer strum-3 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :strum* 1.0 :amp* 1.0 :amp-blur 0.0 :direction down)
		    (:time 0.000 :key 0 :chord [maj]  :inversion 0 :octave 0)
		    (:time 1.000 :key 0 :chord [maj]  :inversion 1 :octave 0)
		    (:time 2.000 :key 0 :chord [maj]  :inversion 1 :octave 1)))
(dump-events strum-3 :filter #'midi-note-off-p)
(terpri)


(format t "strum-4 strum directions~%")
(strummer strum-4 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :direction (down up))
		    (:time 0.000 :key 10 :chord [maj])
		    (:time 1.000 :key 20)
		    (:time 2.000 :key 10)
		    (:time 3.000 :key 20)))
(dump-events strum-4 :filter #'midi-note-off-p)
(terpri)


(format t "strum-5 strum directions dice~%")
(strummer strum-5 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :direction dice)
		    (:time 0.000 :key 10 :chord [maj])
		    (:time 1.000 :key 10)
		    (:time 2.000 :key 10)
		    (:time 3.000 :key 10)))
(dump-events strum-5 :filter #'midi-note-off-p)
(terpri)

(format t "strum-6 strum directions random~%")
(strummer strum-6 piano1
	  :events '((:inversion 0 :octave 0 :strum 0.0 :direction random)
		    (:time 0.000 :key 10 :chord [maj])
		    (:time 1.000 :key 10)
		    (:time 2.000 :key 10)
		    (:time 3.000 :key 10)))
(dump-events strum-6 :filter #'midi-note-off-p)
(terpri)



(format t "strum-7 strum delay no acceleration or amp scale :end-together is false.~%")
(strummer strum-7 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together no :dur 100)
		    (:time 0.000 :key 10 :chord [maj] :strum 0.0)
		    (:time 1.000 :key 10 :strum 1.0)))
(dump-events strum-7)
(terpri)


(format t "strum-7 strum delay no acceleration or amp scale :end-together no.~%")
(strummer strum-7 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together no :dur 100)
		    (:time 0.000 :key 10 :chord [maj] :strum 1.0)))
(dump-events strum-7)
(terpri)


(format t "strum-8 strum delay no acceleration or amp scale :end-together yes.~%")
(strummer strum-8 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together yes :dur 100)
		    (:time 0.000 :key 10 :chord [maj] :strum 1.0)))
(dump-events strum-8)
(terpri)

(format t "strum-9 strum delay acceleration = 1.1, no amp scale :end-together yes.~%")
(strummer strum-9 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together yes :dur 100)
		    (:time 0.000 :key 10 :chord (0 1 2 3) :strum 1.0 :strum* 1.1)))
(dump-events strum-9)
(terpri)


(format t "strum-10 strum delay acceleration = 0.9, no amp scale :end-together yes.~%")
(strummer strum-10 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together yes :dur 100)
		    (:time 0.000 :key 10 :chord (0 1 2 3) :strum 1.0 :strum* 0.9)))
(dump-events strum-10)
(terpri)


(format t "strum-11 strum delay acceleration = 1.0, amp scale = 0.9  :end-together yes.~%")
(strummer strum-11 piano1
	  :events '((:inversion 0 :octave 0 :direction down :end-together yes :dur 100)
		    (:time 0.000 :key 10 :chord [maj] :strum 1.0 :strum* 1.0 :amp* 0.9)))
(dump-events strum-11)
(terpri)


(format t "strum-13 strum amp limits~%")
(strummer strum-12 piano1
	  :events '((:amp-limits 0.3 0.7 :cres 0.1 0.9 12 :chord [solo])
		    (:time 0.100 :key 10)
		    (:time 0.200 :key 10)
		    (:time 0.300 :key 10)
		    (:time 0.400 :key 10)
		    (:time 0.500 :key 10)
		    (:time 0.600 :key 10)
		    (:time 0.700 :key 10)
		    (:time 0.800 :key 10)
		    (:time 0.900 :key 10)
		    (:time 1.000 :key 10)
		    (:time 1.100 :key 10)
		    (:time 1.200 :key 10)
		    (:time 1.300 :key 10)))
(dump-events strum-12 :filter #'midi-note-off-p)


