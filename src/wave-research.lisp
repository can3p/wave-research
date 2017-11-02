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

(defmethod analyze-buffer ((analyzer <spectrum-analyzer>) buffer)
  (check-buffer-for-spike buffer analyzer))

(defun analyze-test-audio ()
  (let ((wave (load-wave (test-file-path))))
    (analyze-wave wave '<spectrum-analyzer>)))

(defun play-test-audio ()
  (let ((wave (load-wave (test-file-path))))
    (analyze-wave wave
                  '<player>
                  '<spectrum-analyzer>)))

(defun analyze-data-from-mic ()
  (let* ((wave (from-mic)))
    (analyze-wave wave '<spectrum-analyzer>)))

(defun plot-test-amplification (&optional (ratio 2))
  (let ((wave (load-wave (test-file-path))))
    (plot-wave (filter-wave wave (list '<amplifier> :ratio ratio)))
