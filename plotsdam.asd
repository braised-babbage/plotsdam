(asdf:defsystem plotsdam
  :author "Erik Davis <erik@cadlag.org>"
  :license "MIT"
  :description "A lightweight interface for Vega Lite plotting."
  :serial t
  :pathname "src/"
  :components ((:file "package")
               (:file "plotsdam"))
  :depends-on (:cl-json :hunchentoot))
