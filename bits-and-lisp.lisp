;;;; bits-and-lisp.lisp

(in-package #:bits-and-lisp)

(defun read-file ()
  (format nil "Read"))

(defun write-file ()
  (format nil "Write"))


(defclass bmp-file ()
  ((path :initform "" :initarg :path :accessor bmp-path)
   (size-bytes :initform 0 :accessor bmp-size-bytes)
   ))

(defun make-bmp-file ()
  (make-instance 'bmp-file))

(defun make-bmp-file-from-path (path)
  "Create a `bmp-file' instance from a file in PATH."
  (let (buffer
        (the-file (make-instance 'bmp-file :path path)))
    (flet ((consume-buffer ()
             "Return the `nreverse' of `buffer', and `setf' it to empty (to start pushing again)."
             (let ((bytes-in-buffer (nreverse buffer)))
               (setf buffer nil)
               bytes-in-buffer)))
      (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
        (loop for value = (read-byte stream nil)
              for position from 0
              do
                 (push value buffer) ;; accumulate until we are at a field limit
                 (cond ((= position 1) (validate-header (consume-buffer)))
                       ((= position 5) (setf (bmp-size-bytes the-file)
                                             (calculate-size (consume-buffer))))
                       ((= position 7) (return)))))
      )
      the-file))

(defun validate-header (header-field)
  "Check that HEADER-FIELD has the values 66 77 (BM in ASCII)."
  (unless (and (= (first header-field) 66)
               (= (second header-field) 77))
    (error "Invalid BMP file - bad header.")))

(defun calculate-size (size-field)
  (byte-seq-to-integer size-field))

;; Slightly modified octets->uint from
;; https://github.com/EuAndreh/cl-intbytes/blob/master/src/cl-intbytes.lisp
;; `ldb' (see PCL chapter 24) gets the bits of an integer given a `byte specifier'
;; this function creates an output integer and sets its bits based on the array of integers
;; pased as parameter, which are all bytes
(defun byte-seq-to-integer (bytes-seq)
  "Interpret BYTES-SEQ as an unsigned integer."
  (let ((bytes-list (coerce bytes-seq 'list))
        (output-int 0))
    (loop for byte-value in bytes-list
          for bit-displacement from 0 by 8 ;; produces 0, 8, 16, 32, etc.
          do
             ;; setting the bits in `output-int' to the bits in the corresponding
             ;; `byte-value', on each iteration `bit-displacement' goes in powers of two
             (setf (ldb (byte 8 bit-displacement) output-int)
                   byte-value))
    output-int))
