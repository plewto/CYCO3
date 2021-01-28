;;;; CYCO examples ex2 section b
;;;; Section B adds a vibraphone to the basic motif.
;;;;

;; Create section B as a clone of A.
;; 
(param b (clone a :new-name "B"))

;; The bulk rename is included only for illustration.  It is needed only if
;; the cloned parts are to be assigned to variables.
;;
(bulk-rename-parts b 1 "B")



(param vibes-cue-list nil)
(dolist (bar '(1 2 3 4 5 6 7 8))
  (dolist (beat '(1 2 3 4))
    (push (list bar beat 1) vibes-cue-list)
    (push (list bar beat 3) vibes-cue-list)))
(setf vibes-cue-list (reverse vibes-cue-list))

(param vibes-key-list (let* ((keys-1 '(f5 a5 f6 c6 f6 c6 a5 f5))
			     (keys-2 (transpose keys-1 1))
			     (keys-3 '(f5 a5 f6 c6 f6 f5 fs5 g5))
			     (keys-4 (transpose keys-2 2))
			     (keys-5 (transpose keys-1 -2)))
			(transpose (append keys-1 keys-2 keys-1 keys-2
					   keys-3 keys-4 keys-5 keys-2)
				   -12)))
(qball b-vibes vibes
       :bars 8
       :cue vibes-cue-list
       :key vibes-key-list
       :amp 'mf)


(->midi b)
(->midi b :filename "loop-b" :repeat 16)
