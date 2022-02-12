;;;; init-segment-entities.lisp

(in-package #:init-segment)

;; using these as guidance for field names & types
;; https://www.cimarronsystems.com/wp-content/uploads/2017/04/Elements-of-the-H.264-VideoAAC-Audio-MP4-Movie-v2_0.pdf
;; http://mp4ra.org/#/qtatom

(defclass init-segment-file ()
  ((path
    :initform nil
    :initarg :path
    :accessor path
    :documentation "Path from which this file was read.")
   (boxes
    :initform nil
    :initarg :boxes
    :accessor boxes
    :documentation "List of MP4 atoms/boxes contained in this file."))
  (:documentation "Representation of an init file. Honestly I just needed something to hold the base
boxes and threw the path in there :P but might be useful to define methods on this. Who knows..."))

(defclass box/atom ()
  ((box-size
    :initform 0
    :initarg :box-size
    :accessor box-size
    :documentation "The size of this atom/box, in bytes.")
   (box-type
    :initform ""
    :initarg :box-type
    :accessor box-type
    :documentation "The type of this atom/box.")
   (raw-data
    :initform nil
    :initarg :raw-data
    :accessor raw-data
    :documentation "The byte array read to parse this atom/box.")
   (child-boxes
    :initform nil
    :initarg :child-boxes
    :accessor child-boxes
    :documentation "List of boxes that are contained within this instance."))
  (:documentation "A base class for all atoms/boxes in an init segment file.
Technically for all type of MPEG types, but for now we are only concerned with init segments."))

(defclass ftyp-box (box/atom)
  ((major-brand
    :initform ""
    :initarg :major-brand
    :accessor major-brand
    :documentation "The type of this file.")
   (minor-version
    :initform 0
    :initarg :minor-version
    :accessor minor-version
    :documentation "The minor version for this file's standard.")
   (compatible-brands
    :initform ()
    :initarg :compatible-brands
    :accessor compatible-brands
    :documentation "List of 4-char elements for compatible file formats."))
  (:documentation "The first box in any valid MPEG file, that identifies the file type plus some
additional metadata."))

(defclass moov-box (box/atom) ()
  (:documentation "Container for all metadata. Doesn't have any own properties, only child-boxes."))

(defun make-ftyp (bytes-seq)
  "Create an `ftyp-box' instance from BYTES-SEQ."
  ;; we know the ftype atom is at the beginning of the file, so, start from 0...
  (let ((ftyp-length (get-segment-int bytes-seq 0 4)))
    ;; validation, this field MUST say "ftyp"
    (unless (string= (get-segment-string bytes-seq 4 4) "ftyp")
      (error "Not a valid ftyp box/atom."))
    (make-instance 'ftyp-box
                   :box-size ftyp-length
                   :box-type "ftyp"
                   :raw-data bytes-seq
                   :major-brand (get-segment-string bytes-seq 8 4)
                   :minor-version (get-segment-int bytes-seq 12 4)
                   ;; field length = (total length - 16 bytes already read), in packs of 4
                   :compatible-brands (loop for index from 16 below ftyp-length by 4
                                            collect (get-segment-string bytes-seq index 4)))))

(defun make-moov (bytes-seq)
  "Create a `moov-box' instance from BYTES-SEQ, populate the first level of sub-boxes."
  (let ((moov-box (make-instance 'moov-box
                                 :box-size (get-segment-int bytes-seq 0 4)
                                 :box-type "moov"
                                 :raw-data bytes-seq))
        ;; skip size + "moov", what follows are child boxes
        (sub-boxes (boxes-in-byte-seq (subseq bytes-seq 8))))
    (setf (child-boxes moov-box)
          (loop for box-data in sub-boxes
                collect (make-moov-sub-box box-data)))
    moov-box))

(defun make-moov-sub-box (sub-box-data)
  "Use SUB-BOX-DATA to create a base box/atom entity.
For the purposes of this code, we just need level 1 below moov, to delete any UUID boxes."
  ;; if there's a need to drill down deeper in the moov sub-levels, here is the place
  (make-instance 'box/atom
                 :box-size (getf sub-box-data :size)
                 :box-type (getf sub-box-data :type)
                 :raw-data (getf sub-box-data :bytes)))
