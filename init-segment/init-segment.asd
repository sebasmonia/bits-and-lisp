;;;; init-segment.asd

(asdf:defsystem #:init-segment
  :description "Parse, and maybe modify, a DASH file init segment."
  :author "Sebastian Monia <workemail@work.com>"
  :license  "Unlicensed"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria
               #:boto3-wrapper
               #:cl-ppcre
               #:uiop)
  :components ((:file "package")
               (:file "init-segment-entities")
               (:file "init-segment")
               (:file "init-segment-cleanup")))
