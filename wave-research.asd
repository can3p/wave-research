(in-package :cl-user)
(defpackage cl-commonmark-asd
  (:use :cl :asdf))
(in-package :cl-commonmark-asd)

(defsystem wave-research
  :name "wave-research"
  :version "0.0.1"
  :license "MIT"
  :author "Dmitry Petrov <dpetroff@gmail.com>"
  :maintainer "Dmitry Petrov <dpetroff@gmail.com>"
  :description "What is it like working with sound in Common Lisp?"
  :homepage "https://github.com/can3p/wave-research"
  :serial T
  :components ((:module "src"
                :components (
                             (:file "package")
                             (:file "read-wav")
                             (:file "wave")
                             (:file "filters")
                             (:file "player")
                             (:file "ring-buffer")
                             (:file "spectrum-check")
                             (:file "helpers")
                             (:file "wave-research"))))
  :depends-on (:cl-portaudio
               :cl-portaudio-tests
               :cl-wav
               :cl-slice
               :cl-arrows
               :eazy-gnuplot
               :bordeaux-fft))
