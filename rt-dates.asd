(defsystem "rt-dates"
  :description "rt-dates: date calculations"
  :version "1.0"
  :author "Richard Todd <richard.wesley.todd@gmail.com>"
  :licence "MIT"
  ;;  :depends-on ("optima.ppcre" "command-line-arguments")
  :serial t
  :components ((:file "packages")
	       (:file "date-calc")))

