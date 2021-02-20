;;;; CYCO examples h
;;;;
;;;; Example h uses an ALLOY generators to transpose basic motif.
;;;; An alloy combines two generators (or numeric-only patterns)
;;;; with a binary function.  The default function is addition.
;;;;

(section h :bars 4 :tempo 120)

(let* ((motif (keynumber '(c4 e4 g4 bf4)))
       (transpose-list '(0 0 0 0 9 9 9 9 5 5 5 5 7 7 7 7))
       (key-list (next (alloy (cycle :of motif)
			      (cycle :of transpose-list) :function #'+)
		       (length transpose-list)))
       (cue-list (create-cue-list :bars 2)))

  (qball h-piano piano
	 :bars 2
	 :cue cue-list
	 :key key-list
	 :dur 'q.))

(->midi h)
