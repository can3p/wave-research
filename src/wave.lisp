(in-package #:wave-research)

(defclass <wave> ()
  (
   (sample-rate :initform +sample-rate+ :reader sample-rate)
   (frames-per-buffer :initform +frames-per-buffer+ :reader frames-per-buffer)))


(defclass <static-wave> (<wave>)
  (
   (audio-data :initarg :audio-data
               :accessor audio-data)))


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



