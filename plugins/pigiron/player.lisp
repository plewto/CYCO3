
;;;; cyco3 pigiron plugin player.lisp
;;;;

(let ((id "player"))
  (defun player-id (&optional new-id)
    (if new-id
	(setf id (->string new-id)))
    id))


(macrolet ((player-command-0 (name command &optional (result-hook #'identity))
	     `(defun ,name (&key (id (player-id))(proxy *default-proxy*) delay)
		(let* ((cmd (sformat "op/~A/~A" id ,command))
		       (message (send proxy cmd))
		       (result (read-osc-response proxy message :delay delay)))
		  (if (eq result :ERROR)
		      :ERROR
		    (funcall ,result-hook (car result))))))
	   
	   (player-command-1 (name command &optional (result-hook #'identity))
	      `(defun ,name (argument &key (id (player-id))(proxy *default-proxy*) delay)
		 (let* ((cmd (sformat "op/~A/~A" id ,command))
			(message (send proxy cmd argument))
			(result (read-osc-response proxy message :delay delay)))
		   (if (eq result :ERROR)
		       :ERROR
		     (funcall ,result-hook (car result)))))))

  (player-command-0 can-record "q-can-record" #'string->bool)
  (player-command-0 is-recording "q-is-recording" #'string->bool)
  (player-command-0 is-playing  "q-is-playing" #'string->bool)
  (player-command-0 current-media "q-current")
  (player-command-0 current-media-url "q-current-url")
  (player-command-0 current-media-duration "q-duration")
  (player-command-0 player-position "q-position")
  (player-command-0 player-relative-position "q-relative-position")
  (player-command-0 clear-media-list "clear")
  (player-command-0 stop "stop")
  (player-command-0 play "play")
  (player-command-0 player-continue "continue")
  (player-command-1 add-media-directory "add-directory")
  (player-command-1 remove-media "remove")
  (player-command-1 select-media "select")
  (player-command-1 seek "seek")

  
  (defun add-media (url &key (id (player-id))(proxy *default-proxy*) delay)
    (let* ((alias (car (split-extension (second (split-path (resolve-user-home url))))))
	   (command (sformat "op/~A/add" id))
	   (message (send proxy command (list alias url)))
	   (result (read-osc-response proxy message :delay delay)))
      result)) )


(labels ((extract-field (record field-name)
	    (let* ((start (search field-name record))
		   (start-value (1+ (search "=" record :start2 start)))
		   (end-value (search "," record :start2 start)))
	      (subseq record start-value end-value)))

	 (parse-media-locator (record alias-only)
	    (let ((alias (extract-field record "alias"))
		  (url (extract-field record "url"))
		  (index (extract-field record "index")))
	      (if alias-only
		  alias
		(list :alias alias :url url :index index))))

	 (parse-media-list (raw-results alias-only)
	    (let ((acc '()))
	      (dolist (item raw-results)
		(push (parse-media-locator item alias-only) acc))
	      (reverse acc)))
	 )

  ;; Retuns one of
  ;;  1) :ERROR
  ;;  2) if alias-only true, list of media aliases
  ;;  3) if alias-only false, nested association list
  ;;        ((:ALIAS a :URL url :index n) ... )
  ;;
  (defun media-list (&key (id (player-id))(proxy *default-proxy*) delay (alias-only t))
    (let* ((command (sformat "op/~A/q-media-list" id))
	   (message (send proxy command))
	   (result (read-osc-response proxy message :delay delay)))
      (if (eq result :ERROR)
	  :ERROR
	(parse-media-list result alias-only)))) )
      
	

    
