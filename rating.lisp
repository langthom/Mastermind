;;;; rating.lisp
;;;; ---------------------------------------------------------------------------
;;;; Author: Thomas Lang
;;;; Version: 1.0, 02/12/2015
;;;; ---------------------------------------------------------------------------
;;;; Description:
;;;; This module contains the definition and functions that form a rating.
;;;; A rating contains as components two integers representing the amount of
;;;; black pins and the amount of white pins.
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

;; Class representating a single rating.
(defclass rating ()
  ((blackCount :initform 0 :initarg :blackCount)
   (whiteCount :initform 0 :initarg :whiteCount))
  (:documentation 
   "Representation of a black-and-white-rating.
    Such a rating contains as components two integers representing the amount of
    black pins and the amount of white pins."))

(defun newRating (blacks whites)
  "Creates a new instance of the class rating and initializes
   its components to the passed values."
  (make-instance 'rating :blackCount blacks :whiteCount whites))

(defun rating-black ((rate rating))
  "Returns the amount of black pins from the rating."
  (slot-value rate 'blackCount))

(defun rating-white ((rate rating))
  "Returns the amount of white pins from the rating."
  (slot-value rate 'whiteCount))

;; Overrides the default implementation of 'print-object' that returns a string 
;; representation of a rating in the form
;;       #<RATING "black: 'b' white: 'w'">
;; where 'b' and 'w' are the slot values for the black (and respectively white)
;; pins of the rating.
(defmethod print-object ((rate rating) out)
  (let ((str "black: "))
    (setf str (concatenate 'string str (write-to-string (rating-black rate))))
    (setf str (concatenate 'string str " white: "))
    (setf str (concatenate 'string str (write-to-string (rating-white rate))))
    (print-unreadable-object (rate out :type t) (format out "~s" str))))
