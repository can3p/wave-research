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

(defun plist-keys (plist)
  (if (null plist) ()
      (cons (car plist) (plist-keys (cddr plist)))))

(defun take (n list)
  (subseq list 0 n))

(defun take-every (n arr)
  (if (typep arr 'vector)
      (loop for i from 0 upto (1- (length arr)) by n
            collect (aref arr i))
      (let ((idx 0)
            (res (list)))
        (loop for item in arr
              do (when (= 0 idx)
                   (push item res))
                 (setf idx (mod (1+ idx) n))
              finally (return (nreverse res))))))

