;;;; CYCO examples ex2 section b1
;;;; Adds vibraphone 
;;;;

;; Create section B1 as a clone of A.
;; 
(param b1 (clone a :new-name "B1"))

;; bulk-rename is included only for illustration.  It is needed only if
;; the cloned parts are to be assigned to variables.
;;
(bulk-rename-parts b1 1 "B1")


;; Construct the vibes-cue-list to produce a note on every
;; eighth note.
;;
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
			(append keys-1 keys-2 keys-1 keys-2
				keys-3 keys-4 keys-5 keys-2)))
(qball b1-vibes vibes
       :bars 8
       :cue vibes-cue-list
       :key vibes-key-list
       :amp 'mf)

;; Adds slight portamento 
;;
(controllers b1-vibes-cc vibes
	     :bars 8
	     :events '((:cc (1 1 1) portamento-time 16)
		       (:cc (1 1 1) portamento 127)))
	     
(->midi b1)
(->midi b1 :filename "loop-b1" :repeat 16)
