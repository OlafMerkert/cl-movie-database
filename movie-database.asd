(defsystem movie-database
  :serial t
  :depends-on ("drakma" "cxml" "closure-html" "css-selectors"
                        "cl-ppcre"
                        "ol-utils")
  :components ((:file "css-select")
               (:file "retrieval")))
