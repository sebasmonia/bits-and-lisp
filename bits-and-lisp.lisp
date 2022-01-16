;;;; bits-and-lisp.lisp

(in-package #:bits-and-lisp)

(defun read-file ()
  (format nil "Read"))

(defun write-file ()
  (format nil "Write"))


(defclass bmp-file ()
  ((path :initarg :path :accessor path)))`

;; I took this from https://github.com/EuAndreh/cl-intbytes/blob/master/src/cl-intbytes.lisp
;; and:
;;
;; (with-open-file (stream "red.bmp" :direction :input :element-type '(unsigned-byte 8))
;;        (loop for value = (read-byte stream nil)
;;              if value
;;                collect value into le-bytes
;;              else
;;                do (return le-bytes)))
;; (octets->uint (coerce (subseq red-data 2 6) 'vector) 4)
;; 750054 (20 bits, #xB71E6)
;;
;; so it seems right, now I just now to understand it :upsidedown:
(octets->uint (coerce (subseq red-data 2 6) 'vector) 4)
(defun octets->uint (array n-bytes &optional (start 0))
  "Interprets `N-BYTES` of a given `ARRAY` as an unsigned integer."
  (let ((int 0))
    (loop
       for byte-position from 0 below (* 8 n-bytes) by 8
       for array-position from 0 below n-bytes
       do (setf (ldb (byte 8 byte-position) int)
                (aref array (+ start array-position))))
    int))
