(asdf:defsystem :trivial-gamekit-input-handler
  :description "Utility class for switching and controlling input in trivial-gamekit"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-gamekit)
  :components ((:file "input-handler")))


(asdf:defsystem :trivial-gamekit-input-handler/example
  :description "trivial-gamekit-input-handler example"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "MIT"
  :depends-on (trivial-gamekit trivial-gamekit-input-handler)
  :components ((:file "example")))
