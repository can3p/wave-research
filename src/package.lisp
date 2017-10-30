(defpackage #:wave-research
  (:use #:cl
        #:portaudio
        #:portaudio-tests
        #:wav
        #:eazy-gnuplot
        #:cl-slice))

(in-package #:wave-research)

(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +frame-duration+ (/ 1.0 +sample-rate+))
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 1)

(defconstant +fft-size+ 1024)
(defconstant +window-size+ 2048)
(defconstant +threshold-window-size+ 11)
(defconstant +threshold-multiplier+ 10)
