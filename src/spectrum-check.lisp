(in-package #:wave-research)

(defclass <spectrum-analyzer> ()
  ((last-prunned-flux :initform 0d0
                      :accessor last-prunned-flux)
   (last-spectrum :initform (make-array +window-size+
                          :element-type 'double-float
                          :initial-element 0.0d0)
                  :accessor last-spectrum)
   (last-flux :initform (make-instance '<ring-buffer>
                                       :buffer-size +window-size+)
              :accessor last-flux)
   (hwindow :initform (hann-window +window-size+)
            :reader hwindow)
   (inner-pad :initform (make-array +window-size+
                          :element-type 'double-float
                          :initial-element 0.0d0)
              :reader inner-pad)
   (first-peak :initform nil :reader first-peak-p)))

(defun mark-peak (analyzer)
  (setf (slot-value analyzer 'first-peak) nil))

(defun get-flux-for-thresholding (analyzer)
    (slice (get-buffer (last-flux analyzer)) (tail +threshold-window-size+)))

(defun mean (arr)
  (/ (reduce '+ arr)
     (length arr)))

(defun contains-peak-p (analyzer spectrum)
  (let ((flux (reduce '+ (map 'vector #'(lambda (a b) (max (- a b) 0))
                              spectrum
                              (last-spectrum analyzer)))))
    (push-buffer (last-flux analyzer) flux)
    (let* ((thresholded (* +threshold-multiplier+
                           (mean (get-flux-for-thresholding analyzer))))
           (prunned (if (< thresholded flux) (- flux thresholded) 0))
           (peak (if (> prunned (last-prunned-flux analyzer)) prunned 0)))
      (setf (last-prunned-flux analyzer) prunned)
      (> peak 0))))

(defun autopower-spectrum (analyzer data)
  (let* ((window-size (length data))
         (windowed (map 'vector #'* data (hwindow analyzer)))
         (padded (concatenate 'vector (inner-pad analyzer) windowed))
         (spectrum (map 'vector #'(lambda (x) (/ x window-size))
                        (bordeaux-fft:sfft padded)))
         (autopower (map 'vector #'(lambda (x y) (abs (* x y)))
                         spectrum
                         (map 'vector #'conjugate spectrum))))
    (slice autopower (cons 0 window-size))))

(defun analyze-data-and-check-peak-p (analyzer data)
  (let* ((spectrum (autopower-spectrum analyzer data)))
    (when (not (first-peak-p analyzer))
      (mark-peak analyzer)
      (contains-peak-p analyzer spectrum))))
