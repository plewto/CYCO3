;;;; CYCO fretworks Bass guitar Chord Model
;;;;

(param *BASS-GUITAR-CHORD-MODEL*
       (fretted-chord-model 'bass-guitar 24 '(e2 a2 d3 g3) :minimum-octave 2))

(define-chord-family *bass-guitar-chord-model* '[solo]
  :e  '((0 (o x x x))
	(0 (x x 2 x))
	(0 (x x x 9))
	(0 (x x x 21)))
  :f  '((0 (1 x x x))
	(0 (x x 3 x))
	(0 (x x x 10))
	(0 (x x x 22)))
  :fs '((0 (2 x x x))
	(0 (x x 5 x))
	(0 (x x x 11))
	(0 (x x x 23)))
  :g  '((0 (3 x x x ))
	(0 (x x x o ))
	(0 (x x x 12))
	(0 (x x x 24)))
  :gs '((0 (4 x x x ))
	(0 (x x x 1 ))
	(0 (x x x 13)))
  :a  '((0 (x o x x ))
	(0 (x x x 2 ))
	(0 (x x x 14)))
  :as '((0 (x 1 x x ))
	(0 (x x x 3 ))
	(0 (x x x 15)))
  :b  '((0 (x 2 x x ))
	(0 (x x x 4 ))
	(0 (x x x 16)))
  :c  '((0 (x 3 x x ))
	(0 (x x x 5 ))
	(0 (x x x 17)))
  :cs '((0 (x 4 x x ))
	(0 (x x x 6 ))
	(0 (x x x 18)))
  :d  '((0 (x x o x))
	(0 (x x x 7))
	(0 (x x x 19)))
  :ef '((0 (x x 1 x))
	(0 (x x x 8))
	(0 (x x x 20))))

(define-chord-family *bass-guitar-chord-model* '[maj]
  :a  '((0 (4 6 6 5))
  	(0 (5 4 2 2))
  	(0 (X o 2 6))
  	(0 (4 3 2 x))
  	(0 (4 o 2 5)))
  :bf '((0 (6 8 o x))
  	(0 (6 8 8 7))
  	(0 (6 5 3 3))
  	(0 (6 8 o x))
  	(0 (6 5 3 x)))
  :b  '((0 (7 9 9 8))
  	(0 (7 6 4 4))
  	(0 (7 6 4 x)))
  :c  '((0 (x 3 2 o))
  	(0 (8 7 5 5))
  	(0 (8 7 5 x))
  	(0 (8 7 10 o))
  	(0 (8 7 5 x))
  	(0 (8 10 10 9)))
  :cs '((0 (9 11 11 10))
  	(0 (x 4 3 1))
  	(0 (9 8 6 6))
  	(0 (9 8 6 x)))
  :d  '((0 (10 o o 11))
  	(0 (10 o 12 11))
  	(0 (9 11 o 11))
  	(0 (10 12 12 11))
  	(0 (x 5 4 2))
  	(0 (10 9 7 7))
  	(0 (10 9 7 x))
  	(0 (10 o 7 x)))
  :ef '((0 (x 6 8 o))
  	(0 (11 12 12 o))
  	(0 (11 13 13 12))
  	(0 (x 6 5 3))
  	(0 (11 19 8 8))
  	(0 (11 10 8 o))
  	(0 (11 10 8 x)))
  :e  '((0 (o 2 2 1))
  	(0 (x 7 6 4))
  	(0 (o 2 6 4)))
  :f  '((0 (1 3 3 2))
  	(0 (x 8 7 5))
  	(0 (1 o 3 5)))
  :fs '((0 (2 4 4 2))
  	(0 (x 9 8 6)))
  :g  '((0 (3 2 o o))
  	(0 (3 2 o x))
  	(0 (3 5 0 4))
  	(0 (3 5 5 4))
  	(0 (x 10 9 7)))
  :af '((0 (4 6 6 5))
  	(0 (x 11 10 8))
  	(0 (4 3 1 1))
  	(0 (4 3 1 x))))


(define-chord-family *bass-guitar-chord-model* '[min]
  :a  '((0 (5 7 7 5))
	(0 (x o 2 5))
	(0 (5 3 2 2))
	(0 (5 3 2 5))
	(0 (5 o 2 5)))
  :bf '((0 (6 8 8 6))
	(0 (6 4 3 3))
	(0 (6 4 3 x)))
  :b  '((0 (7 9 9 7))
	(0 (7 9 o x))
	(0 (7 5 4 4))
	(0 (7 5 4 x))
	(0 (7 9 o x)))
  :c  '((0 (x 3 1 o))
	(0 (8 10 10 8))
	(0 (8 6 5 5))
	(0 (8 6 5 x))
	(0 (8 6 10 o)))
  :df '((0 (9 11 11 9))
	(0 (x 4 2 1))
	(0 (9 7 6 6))
	(0 (9 7 6 x)))
  :ef '((0 (11 13 13 11))
	(0 (x 6 4 3))
	(0 (11 9 8 8))
	(0 (11 9 8 x)))
  :e  '((0 (o 2 2 o))
	(0 (x 7 9 o))
	(0 (o 2 5 x))
	(0 (o 2 5 4))
	(0 (x 7 5 4)))
  :f  '((0 (1 3 3 1))
	(0 (x 8 6 5)))
  :fs '((0 (2 4 4 2))
	(0 (x 9 7 6))
	(0 (2 o 4 6)))
  :g  '((0 (3 1 o o ))
	(0 (3 1 o x))
	(0 (3 5 5 3))
	(0 (3 5 o 3))
	(0 (x 10 8 7)))
  :af '((0 (4 6 6 4))
	(0 (x 11 9 8))
	(0 (4 2 1 1))
	(0 (4 2 1 x))))