(defpackage :othello
  (:use :cl :mini-fiveam :clog :utils)
  (:export))

(in-package :othello)

(defconstant +all-directions+ '(-11 -10 -9 -1 1 9 10 11))

(deftest +all-directions+.1
  (is (= 8 (length +all-directions+))))

(defconstant +empty+ 0 "An empty square")
(defconstant +black+ 1 "An black piece")
(defconstant +white+ 2 "A white piece")
(defconstant +outer+ 3 "Marks squares outside the 8x8 board")

(deftype piece () `(integer ,+empty+ ,+outer+))

(deftest piece.1
  (is (eql nil (typep 123 'piece)))
  (is (eql t (typep 0 'piece)))
  (is (eql t (typep 1 'piece)))
  (is (eql t (typep 2 'piece)))
  (is (eql t (typep 3 'piece))))

(defun piece-name (piece)
  (char ".@0?" piece))

(defun name-piece (name)
  (ecase name
    (#\. +empty+)
    (#\@ +black+)
    (#\0 +white+)
    (#\? +outer+)))

(deftest piece-name.1
  (is (equal #\. (piece-name +empty+)))
  (is (equal #\@ (piece-name +black+)))
  (is (equal #\0 (piece-name +white+)))
  (is (equal #\? (piece-name +outer+))))

(defun opponent (player)
  (if (eql player +black+)
      +white+
      +black+))

(deftype board () '(simple-array piece (100)))

(defun bref (board square)
  (aref board square))
(defun set-bref (board square value)
  (setf (aref board square) value))
;; (defsetf bref (board square) (value)
;;   `(setf (aref ,board ,square) ,value))
(defsetf bref set-bref)

(deftest bref.1
  (let ((board (initial-board)))
    (is (equal +empty+ (bref board 34)))
    (setf (bref board 34) +black+)
    (is (equal +black+ (bref board 34)))))

(defun copy-board (board)
  (copy-seq board))

(defconstant +all-squares+
  (loop for i from 11 to 88
        when (<= 1 (mod i 10) 8)
          collect i))

(defun allocate-board ()
  (make-array 100
              :element-type 'piece
              :initial-element +outer+))

(defun initial-board ()
  (let ((board (allocate-board)))
    (dolist (square +all-squares+)
      (setf (bref board square) +empty+))
    (setf (bref board 44) +white+ (bref board 45) +black+
          (bref board 54) +black+ (bref board 55) +white+)
    board))

(defconstant +initial-board-string+
  ". . . . . . . .
. . . . . . . .
. . . . . . . .
. . . 0 @ . . .
. . . @ 0 . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
")

(defconstant +after-34-move-from-black-string+
  ". . . . . . . .
. . . . . . . .
. . . @ . . . .
. . . @ @ . . .
. . . @ 0 . . .
. . . . . . . .
. . . . . . . .
. . . . . . . .
")

(defun print-board (board)
  (loop for row from 1 to 8
        do (loop for col from 1 to 8
                 for piece = (bref board (+ col (* 10 row)))
                 do (format t "~C" (piece-name piece))
                 unless (eql 8 col)
                   do (format t " "))
        do (format t "~%")))

(defun print-board-to-string (board)
  (with-output-to-string (*standard-output*)
    (print-board board)))

(defun print-board-to-console (board)
  (clog (jscl::lisp-to-js (print-board-to-string board))))

(deftest print-board.1
  (is (equal +initial-board-string+
             (print-board-to-string (initial-board)))))

(defun count-difference (player board)
  "Count player's pieces minus opponent's pieces."
  (- (count player board)
     (count (opponent player) board)))

(deftest count-difference.1
  (let ((board (initial-board)))
    (is (eql 0 (count-difference +black+ board)))
    (is (eql 0 (count-difference +white+ board)))))

(deftest count-difference.2
  (let ((board (parse-board +after-34-move-from-black-string+)))
    (is (eql 3 (count-difference +black+ board)))
    (is (eql -3 (count-difference +white+ board)))))

(defun parse-board (string)
  (let ((board (allocate-board)))
    (with-input-from-string (in string)
      (loop for row from 1 to 8
            do (loop for col from 1 to 8
                     for piece = (name-piece
                                  (prog1
                                      (read-char in)
                                    (read-char in)))
                     do (setf (bref board (+ col (* 10 row)))
                              piece))))
    board))

(deftest parse-board.1
  (is (equal +initial-board-string+
             (print-board-to-string
              (parse-board +initial-board-string+)))))

(defun valid-p (move)
  "Valid moves are numbers in the range 11-88 that end in 1-8."
  (and (integerp move)
       (<= 11 move 88)
       (<= 1 (mod move 10) 8)))

(deftest valid-p.1
    (is (equal '(11 12 13 14 15 16 17 18
                 21 22 23 24 25 26 27 28
                 31 32 33 34 35 36 37 38
                 41 42 43 44 45 46 47 48
                 51 52 53 54 55 56 57 58
                 61 62 63 64 65 66 67 68
                 71 72 73 74 75 76 77 78
                 81 82 83 84 85 86 87 88)
               (remove-if-not #'valid-p (range 0 100)))))

(defun legal-p (move player board)
  "A legal move must be into an empty square, and it must flip at least
one opponent piece."
  (and (eql (bref board move) +empty+)
       (some (lambda (dir)
               (would-flip? move player board dir))
             +all-directions+)))

(deftest legal-p.1
  (is (eql t (legal-p 34 +black+ (initial-board))))
  (is (eql nil (legal-p 36 +black+ (initial-board)))))

(defun make-move (move player board)
  "Update board to reflect move by player."
  (setf (bref board move) player)
  (dolist (dir +all-directions+)
    (make-flips move player board dir))
  board)

(deftest make-move.1
  (let ((board (initial-board)))
    (setq board
          (make-move 34 +black+ board))
    (is (equal +after-34-move-from-black-string+
               (print-board-to-string board)))))

(defun make-flips (move player board dir)
  "Make any flips in the given direction."
  (let ((bracketer (would-flip? move player board dir)))
    (when bracketer
      (loop for c from (+ move dir) by dir until (eql c bracketer)
            do (setf (bref board c) player)))))

(defun would-flip? (move player board dir)
  "Would this move result in any flips in this direction?"
  (let ((c (+ move dir)))
    (and (eql (bref board c) (opponent player))
         (find-bracketing-piece (+ c dir) player board dir))))

(defun find-bracketing-piece (square player board dir)
  "Return the square number of the bracketing piece."
  (cond
    ((eql (bref board square) player)
     square)
    ((eql (bref board square) (opponent player))
     (find-bracketing-piece (+ square dir) player board dir))
    (t nil)))

(defun legal-moves (player board)
  "Returns a list of legal moves for player."
  (loop for move in +all-squares+
        when (legal-p move player board) collect move))

(deftest legal-moves.1
  (is (equal '(34 43 56 65)
             (legal-moves +black+ (initial-board)))))
