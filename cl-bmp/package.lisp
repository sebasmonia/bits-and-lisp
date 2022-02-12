;;;; package.lisp
(in-package #:cl-user)

(defpackage #:cl-bmp
  (:nicknames "bmp" :bmp)
  (:use #:common-lisp)
  (:local-nicknames (:alex :alexandria)
                    ;; more nicknamed packages here
                    )
  (:import-from :alexandria)
  (:import-from :uiop)
  (:export
   #:read-file
   #:write-file))
