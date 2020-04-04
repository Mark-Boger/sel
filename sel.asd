;;;; sel.asd

(asdf:defsystem #:sel
  :description "Describe sel here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:drakma
               :alexandria
               :qbase64)
  :components ((:file "package")
               (:file "sel")))
