;;;; shell.lisp
;;;; ---------------------------------------------------------------------------
;;;; Author: Thomas Lang
;;;; Version: 1.0, 02/12/2015
;;;; ---------------------------------------------------------------------------
;;;; Description:
;;;; This module contains the shell user interface that does the communication
;;;; between the user and a game of Mastermind.
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

;; Loading all required modules.
(load "constants")
(load "mastermind")
(load "colorcode")
(load "rating")

;; Indicator if the game loop has ended or not.
(defparameter *done* nil)

;; Global game variable.
(defparameter *game* (newMastermind))

;; Indicator if the game is over or not.
(defparameter *gameOver* nil)

(defun main ()
  "Main game loop that prints the prompt, reads the
   user input and performs the desired commands. "
  (loop
    (format t "mastermind> ")
    (setf input (read-line))
    (dispatch input)
    (when *done* (return))))

(defun dispatch (input)
  "Dispatcher that performs commands depending on the 
   user-passed command word. "
  (cond ((string-equal ""       input) (merror "No command"))
        ((string-equal "quit"   input) (setf *done* t))
        ((string-equal "help"   input) (print-help))
        ((string-equal "new"    input) (progn (setf *game* (newMastermind)) (proc-new *game*)))
        ((string-equal "switch" input) (progn (switch-guesser *game*) (proc-new *game*)))
        ((starts-with-prefix input "move") (proc-move input))
        ((starts-with-prefix input "eval") (proc-eval input))
        (t (merror "Unknown command"))))

(defun merror (message)
  "Prints an error message with the prefix 'Error! ' and the passed 'message'."
  (format t "Error! ~a~%" message))

(defun starts-with-prefix (str prefix)
  "Checks if 'str' has the prefix 'prefix'."
  (if (< (length str) (length prefix))
      nil
      (let ((yes t))
        (loop for i from 0 to (- (length prefix) 1) do
              (if (not (char-equal (char str i) (char prefix i)))
                  (progn
                    (setf yes nil)
                    (return nil))))
        yes)))

(defun print-help ()
  "Displays the help messages that is stored int the external
   file 'help.txt' by reading and printing it line-by-line. "
  (let ((in (open "./help.txt" :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
            while line do (format t "~a~%" line))
      (close in))))

(defun proc-new ((m mastermind))
  "Performs the rest of the command 'NEW', which sets the *gameOver* variable
   to false and performs a machine move if the machine is the current guesser."
  (setf *gameOver* nil)
  (if (isMachineGuessing *game*)
      (let ((machineTry (machineMove *game*)))
        (format t "machine guess: ~a~%" machineTry))))

(defun split-into-tokens (input)
  "Splits up the string 'input' into a list of tokens using blank spaces as the
   delimiter that separates them."
  (loop for start = 0 then (1+ finish)
        for finish = (position #\Space input :start start)
        collecting (subseq input start finish)
        until (null finish)))

(defun proc-move (input)
  "Performs the command 'MOVE' which does a human move. To do so it reads the
   color code from the user, makes the move and gets the rating. But if the
   human is not the current guesser of if too few parameters were passed, an
   appropriate error message will be printed."
  (let ((tokens (split-into-tokens input)))
    (cond ((isMachineGuessing *game*) (merror "Invalid guesser"))
          ((< (length tokens) 5) (merror "Too few arguments."))
          (*gameOver* (merror "Game over."))
          (t 
           (let ((move (newColorCode)))
             (loop for i from 1 to *num-slots* do
                   (colorcode-set move (- i 1) (parse-integer (nth i tokens))))
             (humanMove *game* move)
             (let ((moveCnt (getMoveCount *game*)))
               (let ((rate (getRating *game* moveCnt)))
                 (format t "~a~%" rate)
                 (cond ((= (rating-black rate) *num-slots*) 
                        (progn
                          (format t "Congratulations! You needed ~D moves~%" moveCnt)
                          (setf *gameOver* t)))
                       ((>= moveCnt *max-moves*)
                        (progn
                          (format t "No more moves - solution: ~A~%" (getSecret *game*))
                          (setf *gameOver* t)))
                       (t t)))))))))

(defun proc-eval (input)
  "Performs the 'EVAL' command, which reads the rating from the user and invokes
   the next machine move. Prints an error message if the human player is 
   guessing, too few command parameters are available or if the game is already
   over."
  (let ((tokens (split-into-tokens input)))
    (cond ((not (isMachineGuessing *game*)) (merror "Invalid guesser."))
          ((< (length tokens) 3)  (merror "Too few parameters."))
          (*gameOver* (merror "Game over."))
          (t
           (let ((blackRating (parse-integer (nth 1 tokens)))
                 (whiteRating (parse-integer (nth 2 tokens))))
             (if (and (= blackRating *num-slots*) (= whiteRating 0))
                 (progn
                   (format t "Wow! I dit it!~%")
                   (setf *gameOver* t))
                 (let ((rate (newRating blackRating whiteRating)))
                   (processEval *game* nil rate)
                   (let ((move (machineMove *game*)))
                     (cond ((equal move nil)
                              (progn 
                                (format t "No possibilities left - you have been cheating!~%")
                                (setf *gameOver* t)))
                           ((>= (- (getMoveCount *game*) 1) *max-moves*)
                              (progn
                                (format t "No more moves - I couldn't find solution.~%")
                                (setf *gameOver* t)))
                           (t (format t "machine guess: ~a~%" move)))))))))))

;; Invoking the main function.
(main)
