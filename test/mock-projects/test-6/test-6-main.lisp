;;;; CYCO test-6  project test  Controllers part.
;;;;

(plugin general-midi)

(project test-6 :bars 100
	 :cuefn #'(lambda (_ time)
		    (dismiss _)
		    (float time)))

(prune-orchestra)
(general-midi-instrument piano :program 'piano1)

(section alpha)


(controllers test-bend piano
	     :events '((:time 0 1 :value 0 1 :steps 8 :type bend)))
(pass? "Controllers :bend"
       (midi-event-list-match-p
	(render-once test-bend)
	(list (cons 0.0000 (midi-pitch-bend  0    0   64))    
	      (cons 0.1429 (midi-pitch-bend  0   18   73))    
	      (cons 0.2857 (midi-pitch-bend  0   36   82))    
	      (cons 0.4286 (midi-pitch-bend  0   54   91))    
	      (cons 0.5714 (midi-pitch-bend  0   73  100))    
	      (cons 0.7143 (midi-pitch-bend  0   91  109))    
	      (cons 0.8571 (midi-pitch-bend  0  109  118))    
	      (cons 1.0000 (midi-pitch-bend  0  127  127)))
	:time-fuzz 1e-4))

(controllers test-pressure piano
	     :events '((:time 0 1 :value 0 1 :steps 8 :type pressure)))
(pass? "Controllers :pressure"
       (midi-event-list-match-p 
	(render-once test-pressure)
	(list (cons 0.0000 (midi-channel-pressure 0   0))
	      (cons 0.1429 (midi-channel-pressure 0  18))
	      (cons 0.2857 (midi-channel-pressure 0  36))
	      (cons 0.4286 (midi-channel-pressure 0  54))
	      (cons 0.5714 (midi-channel-pressure 0  72))
	      (cons 0.7143 (midi-channel-pressure 0  90))
	      (cons 0.8571 (midi-channel-pressure 0 108))
	      (cons 1.0000 (midi-channel-pressure 0 127)))
	:time-fuzz 1e-4))


(controllers test-control-change-1 piano
	     :events '((:time 0 1 :value 0 1 :steps 8 :type 1)))
(pass? "Controllers control-change"
       (midi-event-list-match-p 
	(render-once test-control-change-1)
	(list (cons 0.0000 (midi-control-change 0 1   0))
	      (cons 0.1429 (midi-control-change 0 1  18))
	      (cons 0.2857 (midi-control-change 0 1  36))
	      (cons 0.4286 (midi-control-change 0 1  54))
	      (cons 0.5714 (midi-control-change 0 1  72))
	      (cons 0.7143 (midi-control-change 0 1  90))
	      (cons 0.8571 (midi-control-change 0 1 108))
	      (cons 1.0000 (midi-control-change 0 1 127)))
	:time-fuzz 1e-4))


(let* ((cc-list (defined-controllers))
       (test-controller (car cc-list))
       (controller-name (car test-controller))
       (controller-number (cdr test-controller)))
  (controllers test-named-controller piano
  	       :events (list (list :time 0 1 :value 0 1 :steps 8 :type controller-name)))
  (pass? (sformat "Controllers named-controller  ~A --> ~A" controller-name controller-number)
  	 (midi-event-list-match-p 
  	  (render-once test-named-controller)
  	  (list (cons 0.0000 (midi-control-change 0 controller-number   0))
  		(cons 0.1429 (midi-control-change 0 controller-number  18))
  		(cons 0.2857 (midi-control-change 0 controller-number  36))
  		(cons 0.4286 (midi-control-change 0 controller-number  54))
  		(cons 0.5714 (midi-control-change 0 controller-number  72))
  		(cons 0.7143 (midi-control-change 0 controller-number  90))
  		(cons 0.8571 (midi-control-change 0 controller-number 108))
  		(cons 1.0000 (midi-control-change 0 controller-number 127)))
  	  :time-fuzz 1e-4)))


(controllers test-to piano
	     :events '((:time 0 1 :value 0 1 :steps 4 :type 1)
		       (:time-to 2 :value-to 0 :type 1)))
(pass? "Controllers time-to and value-to"
       (midi-event-list-match-p
	(render-once test-to)
	(list (cons 0.0000 (midi-control-change 0  1   0))
	      (cons 0.3333 (midi-control-change 0  1  42))
	      (cons 0.6667 (midi-control-change 0  1  84))
	      (cons 1.0000 (midi-control-change 0  1 127))
	      (cons 1.0000 (midi-control-change 0  1 127))
	      (cons 1.3333 (midi-control-change 0  1  84))
	      (cons 1.6667 (midi-control-change 0  1  42))
	      (cons 2.0000 (midi-control-change 0  1   0)))
	:time-fuzz 1e-4))


(controllers test-curve piano
	     :curve #'(lambda (q)(limit q 0.2 0.8))
	     :events '((:time 0 1 :steps 8 :type 1)))
(pass? "Controllers curve"
       (midi-event-list-match-p
	(render-once test-curve)
	(list (cons 0.0000 (midi-control-change 0 1  25))
	      (cons 0.1429 (midi-control-change 0 1  25))
	      (cons 0.2857 (midi-control-change 0 1  36))
	      (cons 0.4286 (midi-control-change 0 1  54))
	      (cons 0.5714 (midi-control-change 0 1  72))
	      (cons 0.7143 (midi-control-change 0 1  90))
	      (cons 0.8571 (midi-control-change 0 1 101))
	      (cons 1.0000 (midi-control-change 0 1 101)))
	:time-fuzz 1e-4))
