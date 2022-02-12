;;;; package.lisp

(defpackage #:init-segment
  (:nicknames "inseg" :in-seg)
  (:use #:common-lisp)
  (:local-nicknames (:alex :alexandria))
  (:import-from :boto3-wrapper) ;; nickname b3
  (:import-from :cl-ppcre) ;; nickname ppcre
  (:import-from :uiop)
  (:export
   #:load-init-file
   #:remove-uuid-from-moov-box
   #:download-and-probe-files
   #:*local-dir*
   #:*bucket*
   ))

(in-package #:init-segment)
