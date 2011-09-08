(defsystem movie-database
  :serial t
  :depends-on ("drakma"
               "closure-html"
               "cl-ppcre"
               "ol-utils"
               "com.gigamonkeys.pathnames"
               "split-sequence"
               "clsql-sqlite3")
  :components ((:file "css-select")
               (:file "load-cookie")
               (:file "retrieval")
               (:file "file-analysis")))
