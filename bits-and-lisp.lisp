;;;; bits-and-lisp.lisp

(in-package #:bits-and-lisp)

(defun read-file ()
  (format nil "Read"))

(defun write-file ()
  (format nil "Write"))

(defclass bmp-file ()
  ((path :initform "" :initarg :path :accessor bmp-path)
   (bmp-size-bytes :initform 0 :accessor bmp-size-bytes)
   (dib-size-bytes :initform nil :accessor dib-size-bytes)
   ))

(defun make-bmp-file ()
  (make-instance 'bmp-file))

(defun make-bmp-file-from-path (path)
  "Create a `bmp-file' instance from a file in PATH."
  (let (buffer
        (pixel-array-start -1) ;; starting value that doesn't break the cond below
        (the-file (make-instance 'bmp-file :path path)))
    (flet ((consume-buffer ()
             "Return the `nreverse' of `buffer', and `setf' it to empty (to start pushing again)."
             (let ((bytes-in-buffer (nreverse buffer)))
               (setf buffer nil)
               bytes-in-buffer)))
      (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
        (loop for value = (read-byte stream nil :eof)
              for position from 0
              do
                 (when (eq value :eof)
                   ;; end of file reached
                   (return))
                 (push value buffer) ;; accumulate until we are at a field limit
                 (cond ((= position 1) (validate-file-header (consume-buffer))) ;; "BM" header
                       ;; 4 bytes for size
                       ((= position 5) (setf (bmp-size-bytes the-file)
                                             (byte-seq-to-integer (consume-buffer))))
                       ;; ;; 4 bytes that are optional and can be ignored
                       ((= position 9) (consume-buffer))
                       ;; 4 bytes for "offset to image data"
                       ((= position 13) (setf pixel-array-start
                                              (byte-seq-to-integer (consume-buffer))))
                       ;; once we reach the end of the file header (previous cond), everything
                       ;; between it and pixel-array-start is "DIB header".
                       ;; We are gonna offload that to another function, passing the instance
                       ;; of bmp-file we are populating.
                       ((= pixel-array-start (+ 13 (length buffer))) (process-DIB-header
                                                                      the-file
                                                                      (consume-buffer))
                                                                     ;; discard this value
                                                                     (setf pixel-array-start -1))
                       ;; more fields here
                       )))
      (values the-file
              ;; useful to see what field comes next, until I'm done
              (nreverse buffer)))))

(defun validate-file-header (header-field)
  "Check that HEADER-FIELD has the values 66 77 (BM in ASCII)."
  (unless (and (= (first header-field) 66)
               (= (second header-field) 77))
    (error "Invalid BMP file - bad header.")))

(defun process-DIB-header (bitmap-file header-bytes)
  "Read HEADER-BYTES and popoulate BITMAP-FILE with its data."
  ;; based off https://en.wikipedia.org/wiki/BMP_file_format#DIB_header_(bitmap_information_header)
  ;; we need to know the header size first to determine which type it is. The wiki lists more
  ;; formats than https://docs.microsoft.com/en-us/windows/win32/gdi/bitmap-header-types, will
  ;; focus on the ones described in the latter link only (code for OS/2 headers in 2022 sounds a bit
  ;; passé...)
  (let ((dib-size (byte-seq-to-integer (subseq header-bytes 0 4))))
    ;; as I test more files, I can add more header types here
    (cond ((= dib-size 40) (extract-data-BITMAPINFOHEADER bitmap-file header-bytes))
          (t (error "Unknown bitmap header")))))

(defun extract-data-BITMAPINFOHEADER (bitmap-file header-bytes)
  "Analyze the bytes of HEADER-BYTES as a \"BITMAPINFOHEADER\" and populate BITMAP-FILE with it."
  (let ((dib-size (byte-seq-to-integer (subseq header-bytes 0 4)))
        (pixel-width (byte-seq-to-integer (subseq header-bytes 4 8)))
        (pixel-height (byte-seq-to-integer (subseq header-bytes 8 12)))
        (color-planes (byte-seq-to-integer (subseq header-bytes 12 14))) ;; "must be 1"
        (bits-per-pixel (byte-seq-to-integer (subseq header-bytes 14 16)))
        (compression-method (byte-seq-to-integer (subseq header-bytes 16 20)))
        (raw-bitmap-size (byte-seq-to-integer (subseq header-bytes 20 24)))
        (horizontal-resolution (byte-seq-to-integer (subseq header-bytes 24 28)))
        (vertical-resolution (byte-seq-to-integer (subseq header-bytes 28 32)))
        (colors-palette (byte-seq-to-integer (subseq header-bytes 32 36)))
        (important-colors (byte-seq-to-integer (subseq header-bytes 36))))
    (print dib-size)
    (print pixel-width)
    (print pixel-height)
    (print color-planes)
    (print bits-per-pixel)
    (print compression-method)
    (print raw-bitmap-size)
    (print horizontal-resolution)
    (print vertical-resolution)
    (print colors-palette)
    (print important-colors)))

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
          for bit-displacement from 0 by 8 ;; produces 0, 8, 16, 24, 32, etc.
          do
             ;; setting the bits in `output-int' to the bits in the corresponding
             ;; `byte-value', on each iteration `bit-displacement' goes in powers of two
             (setf (ldb (byte 8 bit-displacement) output-int)
                   byte-value))
    output-int))
