(in-package #:wave-research)


(defclass <ring-buffer> ()
  (buffer
   (idx :initform -1)))

(defmethod initialize-instance :after ((buffer <ring-buffer>) &key (buffer-size 10))
  (setf (slot-value buffer 'buffer) (make-array buffer-size
                                                :element-type 'double-float
                                                :initial-element 0.0d0)))

(defun push-buffer (buffer value)
  (let* ((arr (slot-value buffer 'buffer))
         (idx (mod (1+ (slot-value buffer 'idx)) (length arr))))
    (setf (aref arr idx) value)
    (setf (slot-value buffer 'idx) idx)))

(defun get-buffer (buffer)
  (let* ((arr (slot-value buffer 'buffer))
         (len (length arr))
         (ret (make-array len
                          :element-type 'double-float
                          :initial-element 0.0d0))
         (idx (slot-value buffer 'idx)))
    (loop for i from 0 upto (- len 1)
          do (setf (aref ret i) (aref arr (mod (+ idx i 1) len)))
          finally
             (return ret))))
