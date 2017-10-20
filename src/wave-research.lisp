(in-package #:wave-research)

(defun in-system-path (name extension)
  (let ((fname (make-pathname :name name :type extension))
        (asdf-location (asdf:system-source-file :wave-research)))
    (merge-pathnames fname asdf-location)))

(defun test-file-path ()
  (let ((fname (make-pathname :name "jumps" :type "wav"))
        (asdf-location (asdf:system-source-file :wave-research)))
    (merge-pathnames fname asdf-location)))

(defun plot-png (pathname)
  (let ((filename (namestring pathname)))
    (swank:eval-in-emacs
     `(slime-media-insert-image (create-image ,filename)
                                ,filename))))

(defun audio-series (data)
  (let ((ts 0.0)
        (idx 0))
    (loop while (< idx (length data))
          collect (list ts (aref data idx))
          do (incf ts +frame-duration+)
             (incf idx 1))))

(defun plot-audio-data (data)
  (let ((series (take-every 100 (audio-series data)))
        (filename (in-system-path "test" "png")))
    (with-plots (*standard-output* :debug nil)
      (gp-setup :output filename)
      (plot (lambda ()
              (loop for p in series
                    do (format t "~&~{~a~^ ~}" p)))
            :using '(1 2)
;;            :every 100
            :with '(lines notitle)))
    filename))

(defun read-test-file ()
  (read-wav-file (test-file-path)
                 :chunk-data-reader (wrap-data-chunk-data-samples-reader)))

(defun read-test-audio-data ()
  (getf (caddr (read-test-file)) :chunk-data))

(defun fill-buffer (buffer source start end)
  (let ((src-len (length source))
        (idx 0))
    (loop while (< (+ idx start) end)
          do (if (>= (+ idx start) src-len)
                 (setf (aref buffer idx) 0.0)
                 (setf (aref buffer idx) (aref source (+ idx start))))
             (incf idx))
    buffer))

(defun play-test-audio ()
  (let* ((frames (read-test-audio-data))
         (buffer-size (* +frames-per-buffer+ +num-channels+))
         (idx 0)
         (max-idx (1- (length frames)))
         (analyzer (make-instance '<spectrum-analyzer>))
         (buffer (make-array buffer-size
                             :element-type 'single-float
                             :initial-element 0.0)))
    (with-audio
      (with-default-audio-stream (astream +num-channels+ +num-channels+
                                  :sample-format +sample-format+
                                  :sample-rate +sample-rate+
                                  :frames-per-buffer +frames-per-buffer+)
        (loop while (< idx max-idx)
              do (fill-buffer buffer frames idx
                              (+ idx buffer-size))
                 (when (contains-peak-p analyzer (autopower-spectrum analyzer buffer))
                   (format t "peak! frame ~s, time ~s ~%" idx (* +num-channels+ (/ idx +sample-rate+))))
                 (write-stream astream buffer)
                 (incf idx buffer-size))))))

(defun analyze-data-from-mic ()
  (let* ((analyzer (make-instance '<spectrum-analyzer>))
         (count 0))
    (with-audio
      (with-default-audio-stream (astream +num-channels+ +num-channels+
                                  :sample-format +sample-format+
                                  :sample-rate +sample-rate+
                                  :frames-per-buffer +frames-per-buffer+)
        (loop while t
              do (let ((buffer (read-stream astream)))
                   (when (contains-peak-p analyzer
                                          (autopower-spectrum analyzer buffer))
                     (incf count)
                     (format t "peak! ~s~%" count))))))))
