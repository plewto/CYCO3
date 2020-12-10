;;;; CYCO examples ex2 b
;;;;

(param b (clone a :new-name "B"))


(let* ((cue-list '())
       (keys-1 '(f5 a5 f6 c6 f6 c6 a5 f5))
       (keys-2 (transpose keys-1 1))
       (keys-3 '(f5 a5 f6 c6 f6 f5 fs5 g5))
       (keys-4 (transpose keys-2 2))
       (keys-5 (transpose keys-2 -2))


       (keys (append keys-1 keys-2 keys-1 keys-2
		     keys-3 keys-4 keys-5 keys-2))
       )
       
  (dolist (bar '(1 2 3 4 5 6 7 8))
    (dolist (beat '(1 2 3 4))
      (push (list bar beat 1) cue-list)
      (push (list bar beat 3) cue-list)))
  (setf cue-list (reverse cue-list))

  
  (qball b-vibes vibes
	 :bars 8
	 :cue cue-list
	 :key keys
	 :amp 'mf)

  (qball b-music-box music-box
	 :bars 8
	 :shift 't.
	 :cue cue-list
	 :key (transpose (invert keys 'f5) 12)
	 :amp 'pp-)
  )
		 
       
       

(->midi b)
(->midi b :filename "practice-b" :repeat 16)
