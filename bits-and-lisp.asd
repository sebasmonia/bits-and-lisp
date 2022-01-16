;;;; bits-and-lisp.asd
(in-package #:cl-user)

(asdf:defsystem #:bits-and-lisp
  :description "An attempt to read and maybe write BMP images"
  :author "Sebastián Monía <smonia@outlook.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:uiop)
  :components ((:file "package")
               (:file "bits-and-lisp")))
