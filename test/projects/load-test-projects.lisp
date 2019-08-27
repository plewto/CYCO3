;;;; load-test-project
;;;;
;;;; To load the test projects you must modify *test-project-directory*
;;;; to the proper location.

(global *test-project-directory* "~/dev/CYCO3/test/projects")

(defun project-pass? (project expected-events &key (time-fuzz 1e-5))
  (let ((actual (render-project project)))
    (pass? (sformat "project ~A" (name project))
	   (midi-event-list-match-p actual expected-events :time-fuzz time-fuzz))))

(defun load-test-project (test-number &optional (remarks ""))
  (let ((project-name (->symbol (sformat "test-~A" test-number))))
    (format t "Loading test project ~A" project-name)
    (format t "  ~A~%" remarks)
    (load-project project-name
		  :project-directory *test-project-directory*)))

(load-test-project 1 "Basics")
(load-test-project 2 "Raw parts")
(load-test-project 3 "QBalls")
(load-test-project 4 "Simple parts")  ;; requires manual inspection
(load-test-project 5 "Strummer")      ;; requires manual inspection 
(load-test-project 6 "Controllers")
