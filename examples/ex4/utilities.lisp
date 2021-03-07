;;;; CYCO examples ex4 utilities
;;;;


;;; Creates cue-list for every beat, and optional eighth notes,
;;; for given number of bars, beats.
;;;
(defun create-cue-list (&key (bars 4)(beats 4)(add-eighths t))
  (let ((acc '()))
    (dolist (br (range 1 (1+ bars)))
      (dolist (bt (range 1 (1+ beats)))
	(push (list br bt 1) acc)
	(if add-eighths (push (list br bt 3) acc))))
    (reverse acc)))


(defun dump-key-list (heading key-list &optional cue-list)
  (format t "~A~%" heading)
  (let ((counter 0))
    (dolist (key key-list)
      (format t "[~2D] " counter)
      (if cue-list
	  (format t "~8A --> " (nth counter cue-list)))
      (format t "~3D ~A~%" key (keyname key))
      (setf counter (1+ counter)))))


