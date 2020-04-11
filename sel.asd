;;;; sel.asd

(asdf:defsystem #:sel
  :description "An interface to webdrivers"
  :author "Mark Boger <93mar.bog@gmail.com>"
  :version "0.0.1"
  :serial t
  :depends-on (:drakma
               :yason
               :alexandria
               :qbase64)
  :components ((:file "package")
               (:file "sel")))
