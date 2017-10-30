(in-package #:wave-research)

(defclass <wave> ()
  (
   (sample-rate :initform +sample-rate+ :reader sample-rate)
   (num-channels :initform 1 :reader num-channels)
   (frames-per-buffer :initform +frames-per-buffer+ :reader frames-per-buffer)))


(defclass <static-wave> (<wave>)
  (
   (audio-data :initarg :audio-data
               :accessor audio-data)))

(defun make-buffer (wave)
  (make-instance '<static-wave>
                 :audio-data (make-array (* (frames-per-buffer wave)
                                            (num-channels wave))
                                         :element-type 'single-float
                                         :initial-element 0.0)))

(defgeneric frame-count (wave))

(defmethod frame-count ((wave <wave>))
  (length (audio-data wave)))

(defgeneric for-each-buffer (wave func))

(defmethod for-each-buffer ((wave <static-wave>) func)
  (let* ((buffer (make-buffer wave))
         (frames (audio-data wave))
         (buffer-size (frame-count buffer))
         (idx 0)
         (max-idx (1- (length frames))))
    (loop while (< idx max-idx)
          do (fill-buffer (audio-data buffer) frames idx
                          (+ idx buffer-size))
             (funcall func buffer)
             (incf idx buffer-size))))

(defun load-wave (source)
  (make-instance '<static-wave>
                 :audio-data (read-audio-data source)))

(defun audio-series (wave &key (ts 0.0))
  (let ((idx 0)
        (data (audio-data wave)))
    (loop while (< idx (length data))
          collect (list ts (aref data idx))
          do (incf ts +frame-duration+)
             (incf idx 1))))

(defgeneric plot-wave (wave))

(defmethod plot-wave ((wave <static-wave>))
  (let ((series (audio-series wave))
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

(defgeneric play-wave (wave))

(defmethod play-wave ((wave <static-wave>))
  (with-audio
    (with-default-audio-stream (astream (num-channels wave) (num-channels wave)
                                :sample-format :float
                                :sample-rate (sample-rate wave)
                                :frames-per-buffer (frames-per-buffer wave))
    (for-each-buffer wave #'(lambda (buffer)
          (write-stream astream (audio-data buffer)))))))


