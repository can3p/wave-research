(in-package wave-research)

(defun read-audio-data (source)
  (-<> source
       (read-wav-file <> :chunk-data-reader
                      (wrap-data-chunk-data-samples-reader))
       (caddr)
       (getf :chunk-data)
       (interleaved-to-arrays 2)
       (slice <> 0 t)))
