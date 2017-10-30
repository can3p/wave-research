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

(defun in-system-path (name extension)
  (let ((fname (make-pathname :name name :type extension))
        (asdf-location (asdf:system-source-file :wave-research)))
    (merge-pathnames fname asdf-location)))

(defun interleaved-to-arrays (array num-values &key (type 'single-float))
  (when (> (mod (length array) num-values) 0)
    (error "Cannot split array, because it's not a multiple of number of values"))

  (let* ((frames (/ (length array) num-values))
         (separated (make-array (list num-values frames)
                                :element-type type)))
    (dotimes (frame frames separated)
      (dotimes (value-idx num-values separated)
        (setf (aref separated value-idx frame)
              (aref array (+ (* frame num-values) value-idx)))))))

(defun fill-buffer (buffer source start end)
  (let ((src-len (length source))
        (idx 0))
    (loop while (< (+ idx start) end)
          do (if (>= (+ idx start) src-len)
                 (setf (aref buffer idx) 0.0)
                 (setf (aref buffer idx) (aref source (+ idx start))))
             (incf idx))
    buffer))

(defun audio-series (data)
  (let ((ts 0.0)
        (idx 0))
    (loop while (< idx (length data))
          collect (list ts (aref data idx))
          do (incf ts +frame-duration+)
             (incf idx 1))))


(defun plot-png (pathname)
  (let ((filename (namestring pathname)))
    (swank:eval-in-emacs
     `(slime-media-insert-image (create-image ,filename)
                                ,filename))))

(defmethod print-object :before ((obj pathname) stream)
  (when (string= "png" (pathname-type obj))
    (plot-png obj)))
