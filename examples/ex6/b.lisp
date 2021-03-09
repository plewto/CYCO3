;;;; CYCO example ex6 B   Bender

(section b :bars 16)


;; In use, BENDER is nearly identical to CONTROLLERS.  There differences
;; are as follows:
;;
;; 1) There is no :ctrl keyword.
;;
;; 2) The :cc keyword, for single events, has been replaced with :bend.
;;
;;       :bend time value
;;
;; 3) The value range is normalized   -1.0 <= value <= +1.0.
;;


(strummer b-organ organ
	  :events '((:time (1 1 1) :cc 7 127 :bend 0.0)
		    (:time (1 1 1) :key c6 :dur w)
		    (:time (2 1 1) :key c6 :dur w+w)
		    (:time (4 1 1) :key d6 :chord [min] :dur w)
		    (:time (5 1 1) :key d7 :chord [maj] :dur w)))


(bender b-bender organ
	:events '(
		  ;; Singel bend events 
		  (:bend (1 2 1) 1.0)
		  (:bend (1 3 1) 0.0)

		  ;; Ramp up
		  (:time (2 1 1)(2 3 1) t :value -1.0 0.0 :ramp)

		  ;; Ramp down
		  (:time < (3 2 1) t :value 1.0 0.0 :ramp)

		  ;; LFO like events
		  (:time (4 1 1)(5 1 1) x :value -0.5 +0.5 :cycles 8 :tri)

		  ;; 
		  (:time (5 1 1) (6 1 1) s :value -1.0 +1.0 :cycles 8 :tri)))
		  
(dump-events b)
(->midi b)
	  

