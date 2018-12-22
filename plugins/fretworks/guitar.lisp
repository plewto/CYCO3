;;;; CYCO fretworks plugin Guita Chord Model
;;;;

(param *GUITAR-CHORD-MODEL*
       (fretted-chord-model 'guitar 24 '(e3 a3 d3 g3 b3 e4) :minimum-octave 3))

(define-chord-family *guitar-chord-model* '[solo]
  :e  '((0 ( o  x  x  x  x  x))
  	(0 ( x  x  2  x  x  x))
  	(0 ( x  x  x  x  x  o))
  	(0 ( x  x  x  x  x 12))
  	(0 ( x  x  x  x  x 24)))
  :f  '((0 ( 1  x  x  x  x  x))
  	(0 ( x  x  3  x  x  x))
  	(0 ( x  x  x  x  x  1))
  	(0 ( x  x  x  x  x 13)))
  :fs '((0 ( 2  x  x  x  x  x))
  	(0 ( x  x  4  x  x  x))
  	(0 ( x  x  x  x  x  2))
  	(0 ( x  x  x  x  x 14)))
  :g  '((0 ( 3  x  x  x  x  x))
  	(0 ( x  x  x  o  x  x))
  	(0 ( x  x  x  x  x  3))
  	(0 ( x  x  x  x  x 15)))
  :gs '((0 ( 4  x  x  x  x  x))
  	(0 ( x  x  x  1  x  x))
  	(0 ( x  x  x  x  x  4))
  	(0 ( x  x  x  x  x 16)))
  :a  '((0 ( x  0  x  x  x  x))
  	(0 ( x  x  x  2  x  x))
  	(0 ( x  x  x  x  x  5))
  	(0 ( x  x  x  x  x 17)))
  :as '((0 ( x  1  x  x  x  x))
  	(0 ( x  x  x  3  x  x))
  	(0 ( x  x  x  x  x  6))
  	(0 ( x  x  x  x  x 18)))
  :b  '((0 ( x  2  x  x  x  x))
	(0 ( x  x  x  x  o  x))
  	(0 ( x  x  x  x  x  7))
  	(0 ( x  x  x  x  x 19)))
  :c  '((0 ( x  3  x  x  x  x))
  	(0 ( x  x  x  1  x  x))
  	(0 ( x  x  x  x  x  8))
  	(0 ( x  x  x  x  x 20)))
  :cs '((0 ( x  4  x  x  x  x))
  	(0 ( x  x  x  2  x  x))
  	(0 ( x  x  x  x  x  9))
  	(0 ( x  x  x  x  x 21)))
  :d  '((0 ( x  x  1  x  x  x))
  	(0 ( x  x  x  x  3  x))
  	(0 ( x  x  x  x  x 10))
  	(0 ( x  x  x  x  x 22)))
  :ds '((0 ( x  x  2  x  x  x))
  	(0 ( x  x  x  x  4  x))
  	(0 ( x  x  x  x  x 11))
  	(0 ( x  x  x  x  x 23))))
  
(define-chord-family *guitar-chord-model* 'dy[oct]
  :e  '((0 ( o  x  2  x  x  x))
	(0 ( x  x  2  x  x  o))
	(0 (12  x  14 x  x  x)))
  :f  '((0 ( 1  x  3  x  x  x))
	(0 ( x  x  3  x  x  1))
	(0 ( x  x 15  x  x 13)))
  :fs '((0 ( 2  x  4  x  x  x))
	(0 ( x  x  4  x  7  x))
	(0 ( x  x 16  x 19  x)))
  :g  '((0 ( 3  x  5  x  x  x))
	(0 ( x  x  x  o  8  x))
	(0 ( x  x  x  o  x 15)))
  :gs '((0 ( 4  x  6  x  x  x))
	(0 ( x  x  x  1  x  4))
	(0 ( x  x  x 13  x 16)))
  :a  '((0 ( x  o  x  2  x  x))
	(0 ( x  x  x  2  x  5))
	(0 ( x  x  x 14  x 17)))
  :as '((0 ( x  1  x  3  x  x))
	(0 ( x  x  x  3  x  6))
	(0 ( x  x  x 15  x 18)))
  :b  '((0 ( x  2  x  4  x  x))
	(0 ( x  x  x  x  o  7))
	(0 ( x  x  x  x  o 19)))
  :c  '((0 ( x  3  x  5  x  x))
	(0 ( x  x  x  5  x  8))
	(0 ( x  x  x 17  x 20)))
  :cs '((0 ( x  4  x  6  x  x))
	(0 ( x  x  x  6  x  9))
	(0 ( x  x  x 18  x 21)))
  :d  '((0 ( x  x  o  x  3  x))
	(0 ( x  x  x  7  x  10)))
  :ds '((0 ( x  x  1  x  4  x))
	(0 ( x  x  x  8  x 11)))) 

(define-chord-family *guitar-chord-model* 'dy[per4]
  :e  '((0 ( o  o  x  x  x  x))
	(0 ( x  x  2  2  x  x))
	(0 ( x  x  x  x  5  5))
	(0 ( x  x  x  x 17 17)))
  :f  '((0 ( 1  1  x  x  x  x))
  	(0 ( x  x  3  3  x  x))
  	(0 ( x  x  x  x  6  6))
	(0 ( x  x  x  x 18 18)))
  :fs '((0 ( 2  2  x  x  x  x))
  	(0 ( x  x  4  4  x  x))
  	(0 ( x  x  x  x  7  7))
	(0 ( x  x  x  x 19 19)))
  :g  '((0 ( 3  3  x  x  x  x))
  	(0 ( x  x  5  5  x  x))
  	(0 ( x  x  x  x  8  8))
	(0 ( x  x  x  x 20 20)))
  :gs '((0 ( 4  4  x  x  x  x))
  	(0 ( x  x  6  6  x  x))
	(0 ( x  x  x  x  9  9))
	(0 ( x  x  x  x 21 21)))
  :a  '((0 ( x  o  o  x  x  x))
  	(0 ( x  x  x  2  3  x))
  	(0 ( x  x  x  x 10 10))
	(0 ( x  x  x  x 22 22)))
  :as '((0 ( x  1  1  x  x  x))
	(0 ( x  x  x  3  4  x))
	(0 ( x  x  x  x 11 11))
	(0 ( x  x  x  x 23 23)))
  :b  '((0 ( x  2  2  x  x  x))
	(0 ( x  x  x  x  o  o))
	(0 ( x  x  x  x 12 12))
	(0 ( x  x  x  x 24 24)))
  :c  '((0 ( 1  3  x  x  x  x)) ;; inveretd
	(0 ( x  3  3  x  x  x))
	(0 ( x  x  x  x  1  1))
	(0 ( x  x  x  x 13 13)))
  :cs '((0 ( 2  4  x  x  x  x)) ;; inverted
	(0 ( x  4  4  x  x  x))
	(0 ( x  x  x  x  2  2))
	(0 ( x  x  x  x 14 14)))
  :d  '((0 ( 3  5  x  x  x  x)) ;; inverted
	(0 ( x  x  o  o  x  x))
	(0 ( x  x  x  x  3  3))
	(0 ( x  x  x  x 15 15)))
  :ds '((0 ( 4  6  x  x  x  x)) ;; inverted
	(0 ( x  x  1  1  x  x))
	(0 ( x  x  x  x  4  4))
	(0 ( x  x  x  x 16 16))))

(define-chord-family *guitar-chord-model* 'dy[per5]
  :e  '((0 ( o  2  x  x  x  x))
	(0 ( x  x  2  4  x  x))
	(0 ( x  x  x  x  5  7))
	(0 ( x  x  x  x 17 19)))
  :f  '((0 ( 1  3  x  x  x  x))
  	(0 ( x  x  3  5  x  x))
  	(0 ( x  x  x  x  6  8))
	(0 ( x  x  x  x 18 20)))
  :fs '((0 ( 2  4  x  x  x  x))
  	(0 ( x  x  4  6  x  x))
  	(0 ( x  x  x  x  7  9))
	(0 ( x  x  x  x 19 21)))
  :g  '((0 ( 3  5  x  x  x  x))
  	(0 ( x  x  5  7  x  x))
  	(0 ( x  x  x  x  8 10))
	(0 ( x  x  x  x 20 22)))
  :gs '((0 ( 4  6  x  x  x  x))
  	(0 ( x  x  6  8  x  x))
	(0 ( x  x  x  x  9 11))
	(0 ( x  x  x  x 21 23)))
  :a  '((0 ( x  o  2  x  x  x))
  	(0 ( x  x  x  2  5  x))
  	(0 ( x  x  x  x 10 12))
	(0 ( x  x  x  x 22 24)))
  :as '((0 ( 1  1  x  x  x  x)) ;; inverteed
	(0 ( x  1  3  x  x  x))
	(0 ( x  x  x  3  6  x))
	(0 ( x  x  x  x 11 13)))
  :b  '((0 ( 2  2  x  x  x  x)) ;; inverted
	(0 ( x  2  4  x  x  x))
	(0 ( x  x  x  x  o  2))
	(0 ( x  x  x  x 12 14)))
  :c  '((0 ( 3  3  x  x  x  x)) ;; inverted
	(0 ( x  3  5  x  x  x))
	(0 ( x  x  x  5  8  x))
	(0 ( x  x  x 17 20  x)))
  :cs '((0 ( 4  4  x  x  x  x)) ;; inverted
	(0 ( x  4  6  x  x  x))
	(0 ( x  x  x  6  9  x))
	(0 ( x  x  x 18 21  x)))
  :d  '((0 ( 5  5  x  x  x  x)) ;; inverted
	(0 ( x  x  o  2  x  x))
	(0 ( x  x  x  x  3  5))
	(0 ( x  x  x  x 15 17)))
  :ds '((0 ( 6  6  x  x  x  x)) ;; inverted
	(0 ( x  x  1  3  x  x))
	(0 ( x  x  x  x  4  6))
	(0 ( x  x  x  x 16 18))))

(define-chord-family *guitar-chord-model* '[maj]
  :e  '((0 ( o  2  2  1  o  o))
	(0 ( x  x  2  4  5  4))
        (0 ( o  7  9  9  9  7))
	(0 ( x  7  6  4  5  4))
	(0 (12 11  9  9  9 12))
        (0 (12 14 14 13 12 12)))
  :f  '((0 ( 1  3  3  2  1  1))
	(0 ( x  x  3  5  6  5))
	(0 ( x  8  7  5  6  5))
	(0 ( x  8 10 10 10  8)))
  :fs '((0 ( 2  4  4  3  2  2))
	(0 ( x  x  4  6  7  6))
	(0 ( x  9  8  6  7  6))
	(0 ( x  9 11 11 11  9)))
  :g  '((0 ( 3  2  o  o  o  3))
	(0 ( 3  5  5  4  3  3))
	(0 ( x  x  5  7  8  7))
	(0 ( x 10  9  7  8  7))
	(0 (10 10 12 12 12 10))) 
  :gs '((0 ( 4  3  1  1  1  4))
	(0 ( 4  6  6  5  4  4))
	(0 ( x 11 10  8  9  8))
	(0 (11 11 13 13 13 11)))
  :a  '((0 ( x  o  2  2  2  o))
	(0 ( 5  7  7  6  5  5))
	(0 ( 5  4  2  2  2  5))
	(0 ( x 12 11  9 10  9))
	(0 ( x  x  7  9 10  9)))
  :as '((0 ( 1  1  3  3  3  1))
	(0 ( 6  8  8  7  6  6))
	(0 ( 6  5  3  3  3  6))
	(0 ( x  x  8 10 11 10)))
  :b  '((0 ( 2  2  4  4  4  2))
	(0 ( 7  9  9  8  7  7))
	(0 ( 7  6  4  4  4  7))
	(0 ( x  x  9 11 12 11)))
  :c  '((0 ( x  3  2  o  1  o))
	(0 ( x  3  2  o  1  3))
	(0 ( 3  3  5  5  5  3))
	(0 ( 8  7  5  5  5  8))
	(0 ( 8 10 10  9  8  8))
	(0 ( x  x 10 12 13 12)))
  :cs '((0 ( x  4  3  1  2  1))
	(0 ( x  4  6  6  6  4))
	(0 ( 9 11 11 10  9  9))
	(0 ( 9  8  6  6  6  9)))
  :d  '((0 ( x  x  o  2  3  2))
	(0 ( 5  5  7  7  7  5))
	(0 (10 12 12 11 10 10))
	(0 (10  9  7  7  7 10)))
  :ds '((0 ( x  6  5  3  4  3))
	(0 ( 6  6  8  8  8  6))
	(0 (11 13 13 12 11 11))
	(0 (11 10  8  8  8 11))))
  
(define-chord-family *guitar-chord-model* '[min]
  :e  '((0 ( o  2  2  o  o  o))
	(0 ( x  7  9  9  8  7))
	(0 ( x  x  2  4  5  3)))
  :f  '((0 ( 1  3  3  1  1  1))
	(0 ( x  8 10 10  9  8))
	(0 ( x  x  3  5  6  4)))
  :fs '((0 ( 2  4  4  2  2  2))
	(0 ( x  9 11 11 10  9))
	(0 ( x  x  4  6  7  5)))
  :g  '((0 ( 3  5  5  3  3  3))
	(0 ( x 10 12 12 11 10))
	(0 ( x  x  5  7  8  6)))
  :gs '((0 ( 4  6  6  4  4  4))
	(0 ( x 11 13 13 12 11))
	(0 ( x  x  6  8  9  7)))
  :a  '((0 ( x  o  2  2  1  o))
	(0 ( 5  7  7  5  5  5))
	(0 ( x  x  7  9 10  8)))
  :as '((0 ( x  1  3  3  2  1))
	(0 ( 6  8  8  6  6  6))
	(0 ( x  x  8 10 11  9)))
  :b  '((0 ( x  2  4  4  3  2))
	(0 ( 7  9  9  7  7  7))
	(0 ( x  x  9 11 12 10)))
  :c  '((0 ( x  3  5  5  4  3))
	(0 ( 8 10 10  8  8  8))
	(0 ( x  x 10 12 13 11)))
  :cs '((0 ( x  4  6  6  5  4))
	(0 ( 9 11 11  9  9  9))
	(0 ( x  x 11 13 14 12)))
  :d  '((0 ( x  x  o  2  3  1))
	(0 ( x  5  7  7  6  5))
	(0 (10 12 12 10 10 10)))
  :ds '((0 ( x  6  8  8  7  6))
	(0 (11 13 13 11 11 11))
	(0 ( x  x  1  3  4  2))))

(define-chord-family *guitar-chord-model* '[maj6]
  :e  '((0 ( x  x  2  4  2  4))
	(0 ( x  7  9  9  9  9))
        (0 (12  x 11 13 12  x)))
  :f  '((0 ( x  x  3  5  3  5))
	(0 ( x  8 10 10 10 10))
	(0 (13  X 12 14 13  X)))
  :fs '((0 ( x  x  4  6  4  6))
	(0 ( x  9 11 11 11 11))
	(0 ( 2  x  1  3  2  x)))
  :g  '((0 ( x  x  5  7  5  7))
	(0 ( x 10 12 12 12 12))
	(0 ( 3  x  2  4  3  x)))
  :gs '((0 ( x  x  6  8  6  8))
	(0 ( x 11 13 13 13 13))
	(0 ( 4  x  3  5  4  x)))
  :a  '((0 ( x  x  7  9  7  9))
	(0 ( x 12 14 14 14 14))
	(0 ( 5  x  4  6  5  x)))
  :as '((0 ( x  1  3  3  3  3))
	(0 ( x  x  8 10  8 10))
	(0 ( 6  x  5  7  6  x)))
  :b  '((0 ( x  2  4  4  4  4))
	(0 ( x  x  9 11  9 11))
	(0 ( 7  x  6  8  7  x)))
  :c  '((0 ( x  3  5  5  5  5))
	(0 ( 8  x  7  9  8  x))
	(0 ( x  x 10 12 10 12)))
  :cs '((0 ( x  4  6  6  6  6))
	(0 ( 9  x  8 10  9  x))
	(0 ( x  x 11 13 11 13)))
  :d  '((0 ( x  5  7  7  7  7))
	(0 (10  x  9 11 10  x))
	(0 ( x  x 12 14 12 14)))
  :ds '((0 ( x  6  8  8  8  8))
	(0 (11  x 10 12 11  x))
	(0 ( x  x  1  3  1  3))))

(define-chord-family *guitar-chord-model* '[maj7]
  :e  '((0 ( x  7  9  8  9  7))
	(0 (12 14 13 13 12 12))
	(0 ( x  7  6  4  4  7))
	(0 ( x  x 14 13 12 11))
	(0 ( x  x  2  4  4  4))
	(0 (12  x 13 13 12  x)))
  :f  '((0 ( x  8 10  9 10  8))
	(0 ( 1  3  2  2  1  1))
	(0 ( x  8  7  5  5  8))
	(0 ( x  x 15 14 13 12))
	(0 ( x  x  3  5  5  5))
	(0 ( 1  x  2  2  1  x)))
  :fs '((0 ( x  9 11 19 11  9))
	(0 ( 2  4  3  3  2  2))
	(0 ( x  9  8  6  6  9))
	(0 ( x  x  4  3  2  1))
	(0 ( x  x  4  6  6  6))
	(0 ( 2  x  3  3  2  x)))
  :g  '((1 ( x  9 11 19 11  9))
	(1 ( 2  4  3  3  2  2))
	(1 ( x  9  8  6  6  9))
	(1 ( x  x  4  3  2  1))
	(1 ( x  x  4  6  6  6))
	(1 ( 2  x  3  3  2  x)))
  :gs '((2 ( x  9 11 19 11  9))
	(2 ( 2  4  3  3  2  2))
	(2 ( x  9  8  6  6  9))
	(2 ( x  x  4  3  2  1))
	(2 ( x  x  4  6  6  6))
	(2 ( 2  x  3  3  2  x)))
  :a  '((3 ( x  9 11 19 11  9))
	(3 ( 2  4  3  3  2  2))
	(3 ( x  9  8  6  6  9))
	(3 ( x  x  4  3  2  1))
	(3 ( x  x  4  6  6  6))
	(3 ( 2  x  3  3  2  x)))
  :as '((4 ( x  9 11 19 11  9))
	(4 ( 2  4  3  3  2  2))
	(4 ( x  9  8  6  6  9))
	(4 ( x  x  4  3  2  1))
	(4 ( x  x  4  6  6  6))
	(4 ( 2  x  3  3  2  x)))
  :b  '((5 ( x  9 11 19 11  9))
	(5 ( 2  4  3  3  2  2))
	(5 ( x  9  8  6  6  9))
	(5 ( x  x  4  3  2  1))
	(5 ( x  x  4  6  6  6))
	(5 ( 2  x  3  3  2  x)))
  :c  '((6 ( x  9 11 19 11  9))
	(6 ( 2  4  3  3  2  2))
	(6 ( x  9  8  6  6  9))
	(6 ( x  x  4  3  2  1))
	(6 ( x  x  4  6  6  6))
	(6 ( 2  x  3  3  2  x)))
  :cs '((7 ( x  9 11 19 11  9))
	(7 ( 2  4  3  3  2  2))
	(7 ( x  9  8  6  6  9))
	(7 ( x  x  4  3  2  1))
	(7 ( x  x  4  6  6  6))
	(7 ( 2  x  3  3  2  x)))
  :d  '((8 ( x  9 11 19 11  9))
	(8 ( 2  4  3  3  2  2))
	(8 ( x  9  8  6  6  9))
	(8 ( x  x  4  3  2  1))
	(8 ( x  x  4  6  6  6))
	(8 ( 2  x  3  3  2  x)))
  :ds '((9 ( x  9 11 19 11  9))
	(9 ( 2  4  3  3  2  2))
	(9 ( x  9  8  6  6  9))
	(9 ( x  x  4  3  2  1))
	(9 ( x  x  4  6  6  6))
	(9 ( 2  x  3  3  2  x))))

(define-chord-family *guitar-chord-model* '[maj9]
  :e  '((0 ( x  7  6  8  7  x))
	(0 (12 11 13 11  x  x))
	(0 ( x  x 14 11 12 11)))
  :f  '((1 ( x  7  6  8  7  x))
	(1 (12 11 13 11  x  x))
	(1 ( x  x 14 11 12 11)))
  :fs '((2 ( x  7  6  8  7  x))
	(0 ( 2  1  3  1  x  x))
	(0 ( x  x  4  1  2  1)))
  :g  '((0 ( x 10  9 11 10  x))
	(0 ( 3  2  4  2  x  x))
	(0 ( x  x  5  2  3  2)))
  :gs '((0 ( x 11 10 12 11  x))
	(0 ( 4  3  5  3  x  x))
	(0 ( x  x  6  3  4  3)))
  :a  '((2 ( x 10  9 11 10  x))
	(2 ( 3  2  4  2  x  x))
	(2 ( x  x  5  2  3  2)))
  :as '((3 ( x 10  9 11 10  x))
	(3 ( 3  2  4  2  x  x))
	(3 ( x  x  5  2  3  2)))
  :b  '((0 ( x  2  1  3  2  x))
	(0 ( 7  6  8  6  x  x))
	(0 ( x  x  9  6  7  6)))
  :c  '((1 ( x  2  1  3  2  x))
	(1 ( 7  6  8  6  x  x))
	(1 ( x  x  9  6  7  6)))
  :cs '((2 ( x  2  1  3  2  x))
	(2 ( 7  6  8  6  x  x))
	(2 ( x  x  9  6  7  6)))
  :d  '((0 ( x  5  4  6  5  x))
	(0 (10  9 11  9  x  x))
	(0 ( x  x 12  9 10  9)))
  :ds '((1 ( x  5  4  6  5  x))
	(1 (10  9 11  9  x  x))
	(1 ( x  x 12  9 10  9))))


(define-chord-family *guitar-chord-model* '[dom7]
  :e  '((0 ( x  7  6  7  5  x))
	(0 ( x  x  2  4  3  4))
	(0 ( x  7  9  7  9  7))
	(0 (12 14 12 13 12 12)))
  :f  '((1 ( x  7  6  7  5  x))
	(1 ( x  x  2  4  3  4))
	(1 ( x  7  9  7  9  7))
	(0 ( 1  3  1  2  1  1)))
  :fs '((2 ( x  7  6  7  5  x))
	(2 ( x  x  2  4  3  4))
	(2 ( x  7  9  7  9  7))
	(1 ( 1  3  1  2  1  1)))
  :g  '((3 ( x  7  6  7  5  x))
	(3 ( x  x  2  4  3  4))
	(3 ( x  7  9  7  9  7))
	(2 ( 1  3  1  2  1  1)))
  :gs '((4 ( x  7  6  7  5  x))
	(4 ( x  x  2  4  3  4))
	(4 ( x  7  9  7  9  7))
	(3 ( 1  3  1  2  1  1)))
  :gs '((4 ( x  7  6  7  5  x))
	(4 ( x  x  2  4  3  4))
	(4 ( x  7  9  7  9  7))
	(3 ( 1  3  1  2  1  1)))
  :a  '((5 ( x  7  6  7  5  x))
	(5 ( x  x  2  4  3  4))
	(5 ( x  7  9  7  9  7))
	(4 ( 1  3  1  2  1  1)))
  :as '((6 ( x  7  6  7  5  x))
	(6 ( x  x  2  4  3  4))
	(6 ( x  7  9  7  9  7))
	(5 ( 1  3  1  2  1  1)))
  :b  '((7 ( x  7  6  7  5  x))
	(7 ( x  x  2  4  3  4))
	(7 ( x  7  9  7  9  7))
	(6 ( 1  3  1  2  1  1)))
  :c  '((8 ( x  7  6  7  5  x))
	(8 ( x  x  2  4  3  4))
	(8 ( x  7  9  7  9  7))
	(7 ( 1  3  1  2  1  1)))
  :cs '((9 ( x  7  6  7  5  x))
	(9 ( x  x  2  4  3  4))
	(9 ( x  7  9  7  9  7))
	(8 ( 1  3  1  2  1  1)))
  :d  '((0  ( x  5  4  5  3  x))
	(0  ( x  x 12 14 13 14))
	(0  ( x  5  7  5  7  5))
	(10 ( 0  2  0  1  0  0)))
  :ds '((1  ( x  5  4  5  3  x))
	(1  ( x  x 12 14 13 14))
	(1  ( x  5  7  5  7  5))
	(11 ( 0  2  0  1  0  0))))
