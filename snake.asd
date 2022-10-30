;;;; snake.asd

(asdf:defsystem #:snake
  :description "A new concept in gaming."
  :author "James Kalyan <mjkalyan@defungames.com>"
  :license  ""
  :version "0.0.1"
  :serial t
  :depends-on (#:claylib #:alexandria)
  :components ((:file "package")
               (:file "snake"))
  :build-operation "program-op"
  :build-pathname "snake"
  :entry-point "snake:main")
