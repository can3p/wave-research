(in-package #:wave-research)

(defun test-file-path ()
  (let ((fname (make-pathname :name "jumps" :type "wav"))
        (asdf-location (asdf:system-source-file :wave-research)))
    (merge-pathnames fname asdf-location)))

(defun check-buffer-for-spike (buffer analyzer)
  (when (contains-peak-p analyzer (autopower-spectrum analyzer
                                                      (audio-data buffer)))
    (format t "peak! frame ~s, time ~s ~%"
            (frame-index buffer) (ts buffer))))

(defun analyze-test-audio ()
  (let ((wave (load-wave (test-file-path)))
        (analyzer (make-instance '<spectrum-analyzer>)))
    (for-each-buffer wave #'(lambda (buffer)
                              (check-buffer-for-spike buffer analyzer)))))

(defun play-test-audio ()
  (let ((wave (load-wave (test-file-path)))
        (analyzer (make-instance '<spectrum-analyzer>)))
    (with-audio
      (with-default-audio-stream (astream (num-channels wave) (num-channels wave)
                                  :sample-format :float
                                  :sample-rate (sample-rate wave)
                                  :frames-per-buffer (frames-per-buffer wave))
        (for-each-buffer wave #'(lambda (buffer)
                                  (check-buffer-for-spike buffer analyzer)
                                  (write-stream astream (audio-data buffer)))
                         )))))

(defun analyze-data-from-mic ()
  (let* ((analyzer (make-instance '<spectrum-analyzer>))
         (wave (from-mic)))
    (for-each-buffer wave
                     #'(lambda (buffer)
                                  (check-buffer-for-spike buffer analyzer)))))
