(defsystem movie-database
  :serial t
  :depends-on ("drakma"
               "closure-html"
               "cl-ppcre"
               "ol-utils")
  :components ((:file "css-select")
               (:file "retrieval")))
