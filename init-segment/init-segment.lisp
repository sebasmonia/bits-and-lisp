;;;; init-segment.lisp
(in-package #:init-segment)

(defgeneric load-init-file (source)
  (:documentation "Load an MPEG init file from SOURCE, either a path or a byte list."))

(defmethod load-init-file ((source string))
  "Interpret SOURCE as a pathname, load the file pointed by it, then load the init segment file."
  (load-init-file (pathname source)))

(defmethod load-init-file ((source pathname))
  "Open the file pointed by SOURCE, then load the init segment file."
  (make-init-segment-file source (read-contents source)))

(defmethod load-init-file ((source list))
  "Using SOURCE, a list of bytes from an init segment file, load its fields."
  (make-init-segment-file nil source))

(defun make-init-segment-file (path bytes-seq)
  "Create an `init-segment-file', walking BYTES-SEQ to parse all the MP4 boxes. PATH can be nil.
For init segment files, we know there are 3 top level boxes: ftyp, free, moov. So only drill down on
the moov ones, the others are flat."
  (let ((init-segment (make-instance 'init-segment-file :path path))
        boxes-read)
    ;; iterate destructuring each plist
    (loop for (nil type nil size nil bytes) in (boxes-in-byte-seq bytes-seq)
          do
             (cond ((string= type "ftyp") (push (make-ftyp bytes) boxes-read))
                   ((string= type "moov") (push (make-moov bytes) boxes-read))
                   (t (error "ooops"))))
    ;; store them in the order read, so we can write them back more easily
    (setf (boxes init-segment) (nreverse boxes-read))
    init-segment))

(defun boxes-in-byte-seq (seq &optional (start-index 0))
  "Extract from SEQ (a byte sequence) the MP4 boxes type, size, and raw data.
The function calls itself recursively until it exhausts SEQ. Each box returned can, in turn,
contain more boxes, the file structure is hierachical."
  (multiple-value-bind (box-size box-type) (box-start-values (subseq seq start-index))
    ;; I used to check for box-size within the limits of seq, but I think this is better
    ;; all box types I need for this comply with this expression
    (when (ppcre:scan "^[A-Za-z0-9_]+$" box-type)
      (let* ((box-bytes (subseq seq start-index (+ start-index box-size)))
             (as-plist (list :type box-type :size box-size :bytes box-bytes))
             (next-start (+ start-index box-size)))
        (if (>= next-start (length seq))
            (list as-plist)
            (concatenate 'list (list as-plist) (boxes-in-byte-seq seq next-start)))))))

(defun read-contents (path)
  "Read PATH, and return a list of all bytes contained in it.
Inefficient, but has proven fast enough for now, so..."
  (with-open-file (stream path
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (loop for value = (read-byte stream nil :eof)
          for position from 1
          when (eq value :eof)
               do (return le-bytes)
          collect value into le-bytes)))

(defun write-contents (path byte-array)
  "Open a file in PATH, and write BYTE-ARRAY to it.
Inefficient, but this proven fast enough too..."
  (format t "Writing ~a bytes" (length byte-array))
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (loop for b in byte-array
          do (write-byte b stream)))
  path)


(defun box-start-values (bytes-seq)
  "Interpret the first 8 bytes of BYTES-SEQ as a MP4 box/atom, return the values.
That means read the first 8 bytes as a size, the next 4 as a string."
  (values (get-segment-int bytes-seq 0 4)
          (get-segment-string bytes-seq 4 4)))

(defun get-segment-int (bytes-seq segment-start segment-length)
  "Interpret a subseq of BYTES-SEQ from SEGMENT-START, of SEGMENT-LENGTH, as a little-endian int."
  (byte-seq-to-integer (subseq bytes-seq
                               segment-start
                               (+ segment-start segment-length))
                       t))

(defun get-segment-string (bytes-seq segment-start segment-length)
  "Interpret a subseq of BYTES-SEQ from SEGMENT-START, of SEGMENT-LENGTH, as a string."
  (with-output-to-string (stream)
    (dolist (int-char-code (subseq bytes-seq
                                   segment-start
                                   (+ segment-start segment-length)))
      (princ (code-char int-char-code) stream))))

;; Slightly modified octets->uint from
;; https://github.com/EuAndreh/cl-intbytes/blob/master/src/cl-intbytes.lisp
;; `ldb' (see PCL chapter 24) gets the bits of an integer given a `byte specifier'
;; this function creates an output integer and sets its bits based on the array of integers
;; pased as parameter, which are all bytes
;; (yak shaving at its finest, I wrote this two weeks ago
;; for https://github.com/sebasmonia/bits-and-lisp)
(defun byte-seq-to-integer (bytes-seq &optional (little-endian nil))
  "Interpret BYTES-SEQ as an unsigned integer."
  (let ((bytes-list (coerce bytes-seq 'list))
        (output-int 0))
    (loop for byte-value in (if little-endian
                                (nreverse bytes-list)
                                bytes-list)
          for bit-displacement from 0 by 8 ;; produces 0, 8, 16, 24, 32, etc.
          do
             ;; setting the bits in `output-int' to the bits in the corresponding
             ;; `byte-value', on each iteration `bit-displacement' goes in powers of two
             (setf (ldb (byte 8 bit-displacement) output-int)
                   byte-value))
    output-int))

(defun integer-to-byte-seq (int byte-count &optional (little-endian nil))
  "Convert INT to a sequence (list) of BYTE-COUNT bytes."
  (let ((bytes  (loop for position from 0 below byte-count
                      collect (ldb (byte 8 (* 8 position)) int))))
    (if little-endian
        (nreverse bytes)
        bytes)))
