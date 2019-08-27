;;;; TEST-4 Simple Parts
;;;;
;;;; test-4 checks operation of simple-part.   It does not automatically
;;;; detect flaws.   MIDI events should be inspected manually.


(plugin 'general-midi)
(project test-4 :tempo 60 :bars 8 :beats 4
	 :project-directory (join-path *test-project-directory* "test-4"))

(prune-orchestra)
(general-midi-instrument piano1)

(section alpha)

(simple-part alpha-piano piano1
	     :events '((:time (1 1 1) :bank 5 10)
		       (:time (1 1 3) :key c5 :dur 0.01 :amp ff :chord [solo])
		       (:time (2 1 1) :key c5 :chord [maj] :inversion 0 :octave 0 )
		       (:time (3 1 1) :key c6 :chord [maj] :inversion 1 :octave 1 )
		       (:time (4 1 1) :cc 4 1.0)
		       (:time (5 1 1) :pressure 0.5)
		       (:time (6 1 1) :bend -1.0)))

(dump-events alpha-piano :filter #'midi-note-off-p)
			     
