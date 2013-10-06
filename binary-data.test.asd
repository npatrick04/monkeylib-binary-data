
(defsystem binary-data.test
  :name "binary-data-test"
  :description "Tests for the binary-data library."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :components
  ((:file "test"))
  :depends-on (binary-data alexandria fiveam flexi-streams))
