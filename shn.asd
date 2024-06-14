;;;; shn.asd

(asdf:defsystem #:shn
  :description "SH notation macro"
  :author "Kim YoungCheol <kyceye@gmail.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :depends-on (#:trivial-arguments)
  :serial t
  :components ((:file "package")
               (:file "shn")))
