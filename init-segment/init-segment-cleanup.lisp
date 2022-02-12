;;;; init-segment-cleanup.lisp
(in-package #:init-segment)

(defvar *local-dir* "/some/local/directory" "Local root for files download")
(defvar *bucket* "REDACTED" "S3 bucket")

(defun download-and-probe-files (file-list)
  "Return an alist (local-path . has-uuid-box) for FILE-LIST, which is expected to be the output of
`boto3:s3-list-items'."
  (loop for file in file-list
        for key = (alex:assoc-value file :key)
        for local-path = (b3:s3-download key
                                         :local-directory *local-dir*
                                         :bucket *bucket*)
        do (format t "Processing: ~a key~%" key)
        collect (cons local-path (remove-uuid-from-moov-box local-path :fix nil))))

(defun remove-uuid-from-moov-box (path &key (fix nil))
  "Load PATH, return the uuid box if present. When FIX is t, update the moov box and save.
In the second case, return nil for no changes, and the path if it was updated."
  (flet ((uuid-box-p (box)
           (when (string= "uuid" (box-type box))
             box)))
    (let* ((parsed-file (load-init-file path))
           ;; I am assuming there are only two boxes. Experience shows this is the case, and if it
           ;; isn't I'm in for bigger problems when adjusting box sizes and write the file back >_>
           (ftyp-box (first (boxes parsed-file)))
           (moov-box (second (boxes parsed-file)))
           (uuid-box (some #'uuid-box-p (child-boxes moov-box))))
      (if (and uuid-box
               fix)
        ;; Because of how we structured the boxes (see `box/atom'), the entire byte array for the
        ;; sub-boxes is contained in the parent. We have to ways to remove the uuid box:
        ;; 1. remove it from the sub-boxes, re-join the child byte arrays, update moov-box size
        ;; 2. strip the byte array of the moov-box, recalculate size, ignore child boxes
        ;; The code in `trim-moov' is approach #2
        (write-contents path
                        (concatenate 'list
                                     (raw-data ftyp-box)
                                     (raw-data (trim-moov moov-box uuid-box))))
        uuid-box))))

(defun trim-moov (original uuid-box)
  "Return a new moov-box from ORIGINAL, removing UUID-BOX from it."
  (let* ((original-bytes (raw-data original))
         ;; get the size of the uuid box
         (new-size (- (box-size original) (box-size uuid-box)))
         (new-size-bytes (integer-to-byte-seq new-size 4 t)))
    ;; create a new moov box, to using the new-size-bytes as "list start"
    (make-moov (concatenate 'list new-size-bytes
                            (subseq original-bytes
                                    ;; adjust the limits to account for the 4 bytes for size
                                    4
                                    new-size)))))
