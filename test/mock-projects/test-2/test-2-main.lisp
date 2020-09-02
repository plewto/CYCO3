;;;; test-2 RAW-PART test
;;;;

(project test-2 :tempo 60 :bars 4 :beats 4
	 :project-directory (join-path *test-project-directory* "test-2"))

(section alpha)

(raw-part alpha-bass :bars 2
	  :render-once nil
	  :events (list (cons 0.0   (midi-note-on  0 60 64))
			(cons 0.125 (midi-note-off 0 60 64))
			(cons 1.0   (midi-note-on  0 63 64))
			(cons 1.125 (midi-note-off 0 63 64))
			(cons 2.0   (midi-note-on  0 67 64))
			(cons 2.125 (midi-note-off 0 67 64))
			(cons 3.0   (midi-note-on  0 72 64))
			(cons 3.125 (midi-note-off 0 72 64))))

(section-order '(alpha))

(let ((expected (list (cons  0.0000 (midi-meta-marker "ALPHA"))
		      (cons  0.0000 (midi-time-signature 4 'Q))
		      (cons  0.0000 (midi-tempo-message 60.0))
		      (cons  0.0000 (midi-note-on     0  60  64))
		      (cons  0.1250 (midi-note-off    0  60  64))
		      (cons  1.0000 (midi-note-on     0  63  64))
		      (cons  1.1250 (midi-note-off    0  63  64))
		      (cons  2.0000 (midi-note-on     0  67  64))
		      (cons  2.1250 (midi-note-off    0  67  64))
		      (cons  3.0000 (midi-note-on     0  72  64))
		      (cons  3.1250 (midi-note-off    0  72  64))
		      (cons  8.0000 (midi-note-on     0  60  64))
		      (cons  8.1250 (midi-note-off    0  60  64))
		      (cons  9.0000 (midi-note-on     0  63  64))
		      (cons  9.1250 (midi-note-off    0  63  64))
		      (cons 10.0000 (midi-note-on     0  67  64))
		      (cons 10.1250 (midi-note-off    0  67  64))
		      (cons 11.0000 (midi-note-on     0  72  64))
		      (cons 11.1250 (midi-note-off    0  72  64)))))
  (project-pass? *project* expected))
