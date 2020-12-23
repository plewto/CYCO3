;;;; CYCO mock project test-7 groups
;;;;

(version 3)
(project test-7 :bars 4
	 :project-directory (join-path *mock-project-directory* "test-7"))

(plugin general-midi)
(prune-orchestra)
(general-midi-instrument piano :channel 1 :program 'piano1)
(general-midi-instrument organ :channel 2 :program 'organ1)
(general-midi-instrument sax   :channel 3 :program 'sax-alto)
(general-midi-instrument oboe  :channel 4)
(general-midi-instrument trumpet :channel 5)
(general-midi-instrument trombone :channel 6)

(section alpha)

(simple-part a-piano piano
	     :events '((:time (1 1 1) :key c5)))

(simple-part a-organ organ
	     :events '((:time (1 1 1) :key c5)))

(simple-part a-sax sax
	     :events '((:time (1 1 1) :key c5)))


(simple-part a-oboe oboe
	     :events '((:time (1 1 1) :key c5)))


(simple-part a-trumpet trumpet
	     :events '((:time (1 1 1) :key c5)))

(simple-part a-trombone trombone
	     :events '((:time (1 1 1) :key c5)))


(group keys (list a-piano a-organ))
(group woods (list a-sax a-oboe))
(group brass (list a-trumpet a-trombone))

(labels ((clear-mutes ()
	   (mute keys    :unmute)
	   (mute woods   :unmute)
	   (mute brass   :unmute)
	   (mute a-piano :unmute)
	   (mute a-organ :unmute)
	   (mute a-sax   :unmute)
	   (mute a-oboe  :unmute)
	   (mute a-trumpet  :unmute)
	   (mute a-trombone :unmute))
	 
	 (remove-non-note-on (events)
	    (remove-if-not #'(lambda (evn)
			       (midi-note-on-p (cdr evn)))
			   events))

	 (reduce-channels (events)
	    (sort
	     (remove-duplicates
	      (mapcar #'(lambda (evn)
			  (1+ (channel-index (cdr evn))))
		      events))
	     #'<))

	 (render-reduce ()
	   (reduce-channels (remove-non-note-on (render-once alpha)))) )
	 
	 
  (progn
    (clear-mutes)
    (pass? "All unmuted, group test 1"
	   (equal (render-reduce) '(1 2 3 4 5 6))))

  (progn
    (clear-mutes)
    (mute keys :solo)
    (pass? "Solo keys, group test 2"
	   (equal (render-reduce) '(1 2))))

  (progn
    (clear-mutes)
    (mute keys :solo)
    (mute woods :unmute)
    (pass? "Unmute group, group test 3"
	   (equal (render-reduce) '(1 2 3 4))))
    
  (progn
    (clear-mutes)
    (mute keys :solo)
    (mute a-trumpet :unmute)
    (pass? "Unmute part, group test 4"
	   (equal (render-reduce) '(1 2 5))))

  (progn
    (clear-mutes)
    (mute keys :solo)
    (mute a-piano :mute)
    (pass? "Mute single within group, group test 5"
	   (equal (render-reduce) '(2)))) )


