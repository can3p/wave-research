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
  (read-wav-file (test-file-path)))
