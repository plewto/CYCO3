;;;; CYCO3 test/cyco-test
;;;;

;; If true failed rest produce an error.
;; otherwise failed test displays message and execution continues
;;
(global *test-fail-as-error* nil)


;; If true passed test do not produce output.
;;
(global *silent-pass* nil)

(setf *enable-banners* nil)


(defun fail (test-name reason)
  (let ((message (sformat ":FAIL: test ~A : ~A" test-name reason)))
    (if *test-fail-as-error*
	(error message)
      (format t "~A~%" message))))
		 
(defun pass (test-name)
  (if (not *silent-pass*)
      (format t "    PASS: test ~A~%" test-name)))

(defun pass? (test-name test-result &optional fail-message)
  (if test-result
      (pass test-name)
    (fail test-name fail-message)))

(defun not-tested (message)
  (format t "    NOT TESTED: ~a~%" message))


(defun load-test (test-name)
  (format t "Loading test ~A~%" test-name)
  (load (join-path "test" test-name :as-file)))

(load-test "01-basics")
(load-test "02-math-utilities")
(load-test "03-string-utilities")
(load-test "04-seq-utilities")
(load-test "05-keynumbers")
(load-test "06-chords")
(load-test "07-dynamics")
(load-test "08-metrics")
(load-test "09-midi-utilities")
(load-test "10-nodes")

;; (load "test/projects/load-test-projects")