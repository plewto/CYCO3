;;;; load-mock-project
;;;;


(global *mock-project-directory* (join-path *cyco-location* "test/mock-projects"))

(format t "~%~%Loading mock projects from ~A~%~%" *mock-project-directory*)


(defun load-mock-project (test-number &optional (remarks ""))
  (let ((project-name (->symbol (sformat "test-~A" test-number))))
    (format t "Loading mock project ~A" project-name)
    (format t "  ~A~%" remarks)
    (load-project project-name :project-directory *mock-project-directory*)))

;; (load-mock-project 1 "Basics")
(load-mock-project 2 "Qball")
