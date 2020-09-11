(asdf:defsystem plotsdam-examples
  :author "Erik Davis <erik@cadlag.org>"
  :license "MIT"
  :description "Examples for plotsdam"
  :serial t
  :pathname "examples/"
  :components ((:file "package")
               (:file "bar-plot")
	       (:file "seattle-weather"))
  :depends-on (#:plotsdam #:cl-csv))
