;;;; CYCO examples ex3-main
;;;;
;;;; Example 3 is a cartoon rendition of "The Unanswered Question"
;;;; by Charles Ives.   Other then initialization and a final 
;;;; clean-up, there is a single 64-bar section called "score".
;;;;
;;;; A PDF version of the score may be obtained from
;;;; https://petruccimusiclibrary.ca/files/imglnks/caimg/8/89/IMSLP05327-Charles_Ives_-_The_Unanswered_Question.pdf
;;;;
;;;; A forward to the score has a better explanation, but briefly there are
;;;; three elements:
;;;;
;;;;  1) String section representing eternal existence.
;;;;  2) A solo instrument (English horn) plays a 5-note motif
;;;;     which repeatedly ask the "Perennial Question of Existence".
;;;;  3) A woodwind quartet which profess an answer, yet never provides one.
;;;;
;;;; With each repetition of the question the answers become more agitated
;;;; and eventually start mocking the question.   The piece ends with the
;;;; question unanswered. 

(version 3)
(plugin general-midi)


;; NOTE: The following block illustrates using a custom cue-function called
;; "TBAR".  The utility of TBAR became so obvious that it has since been 
;; included as a standard CYCO function.  The version of TBAR developed 
;; below is slightly different then the new standard version. 
;;

;; Due to rhythmic complexity, particularly with the woodwind "answers",
;; an alternative cueing-function is used.   See the "cue and shuffle
;; functions" section of the documentation.   The default BAR function can
;; handle triplets but is cumbersome.   Instead the TBAR function below
;; is used.   TBAR behaves exactly like BAR -unless- the first element of
;; the time-cue is the symbol 't, in which case it switches to eighth-note
;; triplet mode.  To summarize, time-cue specifications for the normal BAR
;; function have the form
;;                          (bar-number beat-number sixteenth-note)
;;
;; Where 1 <= bar-number
;; 1 <= beat-number <= 4
;; 1 <= sixteenth-note <= 4
;;
;; If the first element is the symbol 't the form is
;;
;;                          (t bar-number triplet-number)
;;
;; 1 <= triplet-number <= 12, the eighth-note triplet within the bar.
;;
;; The :cuefn argument to project sets TBAR for use throughout the project.
;;

(defun tbar (time-signature time-cue)
  (if (eq (car time-cue) 't)
      (let ((bar-number (or (second time-cue) 1))
	    (triplet-number (or (third time-cue) 1)))
	(+ (* (1- bar-number) (bar-duration time-signature))
	   (* 0.5 (1- triplet-number) (tbeat-duration time-signature))))
    (bar time-signature time-cue)))
  

;; Create project
;;
(project ex3
	 :cuefn #'tbar
	 :tempo 70      ;; The specified tempo is ~50, nice for         	 
	 :bars 64       ;; contemplative listening but painfully
	 :beats 4       ;; slow for a tutorial. Feel free to speed it up.
	 :title "The Unanswered Question")

;; Load support files
;;
(lpf orchestra)
(lpf preroll)
(lpf fin)


;; Establish the one and only section.
;;
(section score :bars 64)
;; (metronome score-metronome)  ;; optional metronome

;; Load the parts files.
;;
(lpf strings)
(lpf question)
(lpf answers)

;; Write main MIDI file.
;;
(section-order '(preroll score reset))

(project->midi)
