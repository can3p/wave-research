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
  :homepage "https://github.com/can3p/cl-commonmark"
  :serial T
  :components ((:module "src"
                :components (
                             (:file "package")
                             (:file "ring-buffer")
                             (:file "helpers")
                             (:file "wave-research"))))
  :depends-on (:cl-portaudio
               :cl-portaudio-tests
               :cl-wav
               :eazy-gnuplot
               :bordeaux-fft))
