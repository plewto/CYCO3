;;;; load-mock-project
;;;;


(global *mock-project-directory* (join-path *cyco-location* "test/mock-projects"))

(format t "~%~%Loading mock projects from ~A~%~%" *mock-project-directory*)

(plugin general-midi)
(plugin guitar-chords)

(defun load-mock-project (test-number &optional (remarks ""))
  (let ((project-name (->symbol (sformat "test-~A" test-number))))
    (format t "Loading mock project ~A" project-name)
    (format t "  ~A~%" remarks)
    (load-project project-name :project-directory *mock-project-directory*)))


(defun filter-time-range (time-signature start end events)
  (let ((mn (bar time-signature start))
	(mx (bar time-signature end)))
    (remove-if-not #'(lambda (evn)(and (<= mn (car evn))
				       (<= (car evn) mx)))
		   events)))

(defun filter-message-type (predicate channel events)
  (remove-if-not #'(lambda (evn)
		     (and (funcall predicate (cdr evn))
			  (= (1- channel)(channel-index (cdr evn)))))
		 events))


(defun ~= (a b &optional (epsilon 0.0001))
  (<= (abs (- a b)) epsilon))

(load-mock-project 1 "Basics")
(load-mock-project 2 "Qball")
(load-mock-project 3 "Controllers")
(load-mock-project 5 "Strummer")
(load-mock-project 6 "Transformer")
(load-mock-project 7 "Groups & mutes")
(load-mock-project 8 "Section order")
(load-mock-project 9 "Parts time shift")


