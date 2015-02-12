;;;; colorcode.lisp
;;;; ---------------------------------------------------------------------------
;;;; Author: Thomas Lang
;;;; Version: 1.0, 02/12/2015
;;;; ---------------------------------------------------------------------------
;;;; Description:
;;;; This module contains the definition and functions for a color code.
;;;; For the individual descriptions see the documentation strings of the class
;;;; and functions.
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

;; Loading required module.
(load "constants")

(defclass colorcode ()
  ((code :initform (make-list *num-slots* :initial-element 0) :initarg :code))
  (:documentation 
   "Representation of a color code.
    A color code is a wrapper around *num-slots* numbers representing colors."))

(defun newColorCode ()
  "Creates a new instance of 'colorcode'."
  (make-instance 'colorcode))

(defun colorcode-get ((cod colorcode) (index integer))
  "Returns the entry of the internal code of 'cod' at 'index'."
  (if (< index 0)
      (error "Invalid index passed")
      (nth index (slot-value cod 'code))))

(defun colorcode-set ((cod colorcode) (index integer) (entry integer))
  "Sets the entry of the internal code of 'cod' at 'index' to 'entry'."
  (if (or (< index 0) (>= index *num-slots*)) 
      (error "Invalid index passed"))
      (setf (nth index (slot-value cod 'code)) entry))

(defun colorcode-createSecret ()
  "Creates a new randomly created colorcode."
  (let ((newSecret (newColorCode)))
    (loop for index from 0 to (- *num-slots* 1) do
          (colorcode-set newSecret index (random *num-colors*)))
    newSecret))

;; Overrides the default implementation of 'print-object' that returns a string 
;; representation of a colorcode in the form
;;        #<COLORCODE 'code'>
;; where 'code' is a string containing all numbers of the internal code
(defmethod print-object ((code colorcode) out)
  (let ((str nil))
    (loop for i from 0 to (- *num-slots* 1) do
          (setf str (concatenate 'string str (write-to-string (colorcode-get code i)))))
    (print-unreadable-object (code out :type t) (format out "~s" str))))
