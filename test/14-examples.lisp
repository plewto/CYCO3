;;;; test 14 examples
;;;;
;;;; Ensure example projects load.

(let ((temp *projects-root*))
  (setf *projects-root* (join-path *cyco-location* "examples"))
  (dolist (project-name '(ex1 ex2 ex3 ex4 ex5 ex6))
    (format t "Loading example project ~A~%" project-name)
    (load-project project-name))
  (setf *projects-root* temp))



