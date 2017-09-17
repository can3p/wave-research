(in-package #:wave-research)

;; class SpectralAnalyser(object):
;;     def __init__(self, window_size, segments_buf=None):
;;         self._window_size = window_size
;;         if segments_buf is None:
;;             segments_buf = int(SAMPLE_RATE / window_size)
;;         self._segments_buf = segments_buf

;;         self._thresholding_window_size = THRESHOLD_WINDOW_SIZE
;;         assert self._thresholding_window_size <= segments_buf

;;         self._last_spectrum = np.zeros(window_size, dtype=np.int16)
;;         self._last_flux = deque(
;;             np.zeros(segments_buf, dtype=np.int16), segments_buf)
;;         self._last_prunned_flux = 0

;;         self._hanning_window = np.hanning(window_size)
;;         # The zeros which will be used to double each segment size
;;         self._inner_pad = np.zeros(window_size)

;;         # To ignore the first peak just after starting the application
;;         self._first_peak = True

;;     def _get_flux_for_thresholding(self):
;;         return list(itertools.islice(
;;             self._last_flux,
;;             self._segments_buf - self._thresholding_window_size,
;;             self._segments_buf))

;;     def find_onset(self, spectrum):
;;         """
;;         Calculates the difference between the current and last spectrum,
;;         then applies a thresholding function and checks if a peak occurred.
;;         """
;;         last_spectrum = self._last_spectrum
;;         flux = sum([max(spectrum[n] - last_spectrum[n], 0)
;;             for n in xrange(self._window_size)])
;;         self._last_flux.append(flux)

;;         thresholded = np.mean(
;;             self._get_flux_for_thresholding()) * THRESHOLD_MULTIPLIER
;;         prunned = flux - thresholded if thresholded <= flux else 0
;;         peak = prunned if prunned > self._last_prunned_flux else 0
;;         self._last_prunned_flux  = prunned
;;         return peak

;;     def process_data(self, data):
;;         spectrum = self.autopower_spectrum(data)

;;         onset = self.find_onset(spectrum)
;;         self._last_spectrum = spectrum

;;         if self._first_peak:
;;             self._first_peak = False
;;             return

;;         if onset:
;;             return True

;;     def autopower_spectrum(self, samples):
;;         """
;;         Calculates a power spectrum of the given data using the Hamming window.
;;         """
;;         # TODO: check the length of given samples; treat differently if not
;;         # equal to the window size

;;         windowed = samples * self._hanning_window
;;         # Add 0s to double the length of the data
;;         padded = np.append(windowed, self._inner_pad)
;;         # Take the Fourier Transform and scale by the number of samples
;;         spectrum = np.fft.fft(padded) / self._window_size
;;         autopower = np.abs(spectrum * np.conj(spectrum))
;;         return autopower[:self._window_size]


(defclass <spectrum-analyzer> ()
  (last-spectrum
   last-flux
   (hwindow :initform (hann-window (* +frames-per-buffer+ +num-channels+))
            :reader hwindow)
   (inner-pad :initform (make-array (* +frames-per-buffer+ +num-channels+)
                          :element-type 'double-float
                          :initial-element 0.0d0)
              :reader inner-pad)
   (first-peak :initform nil :reader first-peak-p)))

(defun mark-peak (analyzer)
  (setf (slot-value analyzer 'first-peak) nil))

(defun contains-peak-p (analyzer spectrum)
  (declare (ignore analyzer spectrum)))

(defun autopower-spectrum (analyzer data)
  (let* ((window-size (length data))
         (windowed (map 'vector #'* data (hwindow analyzer)))
         (padded (concatenate 'vector (inner-pad analyzer) windowed))
         (spectrum (map 'vector #'(lambda (x) (/ x window-size))
                        (bordeaux-fft:sfft padded)))
         (autopower (map 'vector #'(lambda (x y) (abs (* x y)))
                         spectrum
                         (map 'vector #'conjugate spectrum))))
    (slice autopower (cons 0 window-size))))

(defun analyze-data-and-check-peak-p (analyzer data)
  (let* ((spectrum (autopower-spectrum analyzer data)))
    (when (not (first-peak-p analyzer))
      (mark-peak analyzer)
      (contains-peak-p analyzer spectrum))))
