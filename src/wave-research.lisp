(defpackage #:wave-research
  (:use #:cl #:portaudio #:portaudio-tests #:wav))

(in-package #:wave-research)

(defun test-file-path ()
  (let ((fname (make-pathname :name "jumps" :type "wav"))
        (asdf-location (asdf:system-source-file :wave-research)))
    (merge-pathnames fname asdf-location)))

(defun plist-keys (plist)
  (if (null plist) ()
      (cons (car plist) (plist-keys (cddr plist)))))

(defun take (n list)
  (subseq list 0 n))

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
    

(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 2)

(defun play-test-audio ()
  (let* ((frames (read-test-audio-data))
         (buffer-size (* +frames-per-buffer+ +num-channels+))
         (idx 0)
         (max-idx (1- (length frames)))
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
                 (write-stream astream buffer)
                 (incf idx buffer-size))))))
