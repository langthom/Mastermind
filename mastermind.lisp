;;;; mastermind.lisp
;;;; ---------------------------------------------------------------------------
;;;; Author: Thomas Lang
;;;; Version: 1.0, 02/12/2015
;;;; ---------------------------------------------------------------------------
;;;; Description:
;;;; This module contains the definition and functions for a game of Mastermind,
;;;; a guessing game from 1970. For description of the game itself or the 
;;;; individual functions, please look at the documentation strings.
;;;; ---------------------------------------------------------------------------
;;;; License:
;;;; Copyright (c) 2015 Thomas Lang
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;;; of this software and associated documentation files (the "Software"), to deal
;;;; in the Software without restriction, including without limitation the rights
;;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;;;; THE SOFTWARE.
;;;; ---------------------------------------------------------------------------

;;; loading required modules
(load "constants")
(load "colorcode")
(load "rating")

(defclass mastermind ()
  ((machineGuessing :initform nil :initarg :machineGuessing)
   (moveCount :initform 0 :initarg :moveCount)
   (secret :initform nil :initarg :secret)
   (guesses :initform nil :initarg :guesses)
   (ratings :initform nil :initarg :ratings)
   (possibilities :initform nil :initarg :possibilities))
  (:documentation 
   "Implementation of a Mastermind game.
    Mastermind is a logical game where one player (either the user or the
    machine) sets a secret color code and the other user has to guess it. If a
    move was taken the guesser gets a response how many colors are identiccal in
    colors and position (black pins) and how many colors are identical in color
    but not in position (white pins). If the user could not guess the code in
    under a certain amount of guesses, he/she will lose. If a game is already 
    over, there will be no other tries."))

(defun newMastermind ()
  "Creates a new Mastermind game by creating a new instance of the
   class 'mastermind' and initializing all attributes to their default values."
  (init-game (make-instance 'mastermind)))

(defun init-game ((m mastermind))
  "Does the initialization of special attributes depending on the actual
   game state: If the human player is guessing, then the machine has to compute
   a secret color code the human guesses against, but if the machine is the
   guesser, then it needs a list of all possible color codes to find the next 
   one."
    (if (isMachineGuessing m)
        (initPermutationsList m)
      (setf (slot-value m 'secret) (colorcode-createSecret))) m)

(defun switch-guesser ((m mastermind))
  "Switches the actual game state by inverting the guesser (from machine to
   human or otherwise) and initializes a new game."
  (setf (slot-value m 'machineGuessing) (not (isMachineGuessing m)))
  (init-game m))

(defun isMachineGuessing ((m mastermind))
  "Checks whether the player of the machine is the current guesser."
  (slot-value m 'machineGuessing))

(defun getMoveCount ((m mastermind))
  "Returns the current amount of moves taken."
  (slot-value m 'moveCount))

(defun getGameState ((m mastermind) (moveNo integer))
  "Returns the move taken at move counter 'moveNo'."
  (if (< moveNo 0) (error "Invalid index passed."))
  (elt (slot-value m 'guesses) (- moveNo 1)))

(defun getRating ((m mastermind) (moveNo integer))
  "Returns the rating of the move with number 'moveNo'."
  (if (< moveNo 0) (error "Invalid index passed."))
  (elt (slot-value m 'ratings) (- moveNo 1)))

(defun getSecret ((m mastermind))
  "Returns the color code that has to be guessed."
  (if (isMachineGuessing m) 
      (error "It's not the machine's turn.")
      (slot-value m 'secret)))

(defun addGuess ((m mastermind) (move colorcode))
  "Adds the passed 'move' to the internal list of guesses."
  (let ((ms (slot-value m 'guesses)))
    (if (equal ms nil)
        (setf (slot-value m 'guesses) (list move))
        (setf (slot-value m 'guesses) (append ms (list move))))))

(defun addRating ((m mastermind) (rate rating))
  "Adds the passed 'rate' to the internal list of ratings."
  (let ((rs (slot-value m 'ratings)))
    (if (equal rs nil)
        (setf (slot-value m 'ratings) (list rate))
        (setf (slot-value m 'ratings) (append rs (list rate))))))

(defun increaseMoveCounter ((m mastermind))
  "Increases the internal move counter by one."
  (setf (slot-value m 'moveCount) (1+ (getMoveCount m))))

(defun humanMove ((m mastermind) (move colorcode))
  "Performs a human move: Compares the passed 'move' with the secret color code
   to guess, adds the computed rating to the internal ratings list and increases
   the move counter."
  (cond ((isMachineGuessing m) (error "It's not the machine's turn"))
        ((>= (- (getMoveCount m) 1) *max-moves*) (error "The game is already over"))
        (t (progn
             (addGuess m move)
             (addRating m (make-rating move (getSecret m)))
             (increaseMoveCounter m)))))

(defun make-rating ((move colorcode) (code colorcode))
  "Computes the black and white rating by comparing the passed 'move' to the 
   passed 'code'. For each code peg there is a black pin set if both the color
   and position are equal, and a white pin is set if the color is contained in
   the code but at the wrong position."
  (let ((blacks 0)
        (whites 0)
        (totals 0)
        
        ;; Lists containing the relative frequencies of the colors in the code
        ;; (or the move, respectively).
        (codecolors (make-list *num-colors* :initial-element 0))
        (guesscolors (make-list *num-colors* :initial-element 0)))
    (loop for i from 0 to (- *num-slots* 1) do
          (let ((curcode (colorcode-get move i))
                (curmove (colorcode-get code i)))
            (setf (nth curcode codecolors) (1+ (nth curcode codecolors)))
            (setf (nth curmove guesscolors) (1+ (nth curmove guesscolors)))
            (if (= curcode curmove) (setf blacks (1+ blacks)))))
    (loop for i from 0 to (- *num-colors* 1) do
          (setf totals (+ totals (min (nth i codecolors) (nth i guesscolors)))))
    (setf whites (- totals blacks))
    (newRating blacks whites)))

(defun machineMove ((m mastermind))
  "Computes the next machine move by the following algorithm:
   The next move is the first move available in the list of all possible moves
   with this list decreased by all moves which has a different rating than
   the passed one which is stored in the internal list of ratings."
  (cond ((not (isMachineGuessing m)) (error "It's not the human's turn"))
        ((>= (- (getMoveCount m) 1) *max-moves*) (error "The game is already over"))
        ((null (slot-value m 'guesses))
           (let ((mmove (car (slot-value m' possibilities))))
             (addGuess m mmove)
             (increaseMoveCounter m)
             mmove))
        (t 
         (let ((lastMove (getGameState m (getMoveCount m)))
               (lastRate (getRating m (getMoveCount m))))
           (let ((blackMove (rating-black lastRate))
                 (whiteMove (rating-white lastRate)))
             (setf (slot-value m 'possibilities)
                   
                   ;; Here we remove all moves that cannot be the solution.
                   ;; These are all possibilities where one or both ratings are
                   ;; different from the rating the user has passed.
                   (remove-if 
                    (lambda (code) 
                      (let ((curRating (make-rating lastMove code)))
                        (let ((black (rating-black curRating))
                              (white (rating-white curRating)))
                          (or
                           (not (equal black blackMove))
                           (not (equal white whiteMove))))))
                    (slot-value m 'possibilities)))
             (increaseMoveCounter m)
             (if (not (null (slot-value m 'possibilities)))
                 (let ((firstOfRest (car (slot-value m 'possibilities))))
                   (addGuess m firstOfRest)
                   firstOfRest)
                 nil))))))

(defun initPermutationsList ((m mastermind))
  "Creates a new list containing all possible color codes that are available
   at the beginning of a machine game."
  (let ((allPossible (expt *num-colors* *num-slots*))
        (permutations nil))
    
    ;; Fill the slots o the color code efficiently by computing the combinations
    ;; as equivalence classes of the numbers to base *num-colors*. This makes
    ;; this initialization independent of changes on the constants *num-colors*
    ;; or *num-slots* stored in the module 'constants'.
    (loop for i from 0 to (- allPossible 1) do
          (let ((newcode (newColorCode))
                (cur i))
            (loop for j from (- *num-slots* 1) downto 0 do
                  (colorcode-set newcode j (mod cur *num-colors*))
                  (setf cur (floor cur *num-colors*)))
            (setf permutations (append permutations (list newcode)))))
    (setf (slot-value m 'possibilities) permutations)))

(defun processEval ((m mastermind) (move colorcode) (rate rating))
  "Processes the 'EVAL' command by updating the internal game state.
   As we already add all guesses (moves) in the machine move function, we do not
   add it here. Furthermore it adds the user passed rating to the internal list
   of all ratings."
  (cond ((not (isMachineGuessing m)) (error "It's not the human's turn"))
        ((>= (- (getMoveCount m) 1) *max-moves*) (error "The game is already over"))
        (t (addRating m rate))))
