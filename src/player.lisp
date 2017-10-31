(in-package wave-research)

(defclass <player> ()
  ((astream :initform nil :accessor astream)))

(defmethod setup-processor ((player <player>) wave)
  (initialize)
  (setf (astream player) (open-default-stream (num-channels wave)
                                             (num-channels wave)
                                             :float
                                             (sample-rate wave)
                                             (frames-per-buffer wave)))
  (start-stream (astream player)))

(defmethod analyze-buffer ((player <player>) buffer)
  (write-stream (astream player) (audio-data buffer)))


(defmethod cleanup-processor ((player <player>))
  (portaudio::stop-stream (astream player))
  (close-stream (astream player))
  (terminate))

(defgeneric play-wave (wave))

(defmethod play-wave ((wave <static-wave>))
  (with-audio
    (with-default-audio-stream (astream (num-channels wave) (num-channels wave)
                                :sample-format :float
                                :sample-rate (sample-rate wave)
                                :frames-per-buffer (frames-per-buffer wave))
    (for-each-buffer wave #'(lambda (buffer)
          (write-stream astream (audio-data buffer)))))))
