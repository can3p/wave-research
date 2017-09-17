(in-package #:wave-research)

(defun hann-window (n)
  (let ((buffer (make-array n
                            :element-type 'double-float
                            :initial-element 0.0d0)))
    (loop for i from 0 upto (- n 1)
          do
             (setf (aref buffer i) (bordeaux-fft:hann i n))
          finally
             (return buffer))))
