(defpackage :md-load-cookie
  (:use :cl :ol-utils :clsql)
  (:export :obtain-imdb-cookie-jar))

(in-package :md-load-cookie)

(file-enable-sql-reader-syntax)

(defparameter *cookie-db*
  (format nil "~A"
          (merge-pathnames
           #P".config/chromium/Default/Cookies"
           (user-homedir-pathname))))

(defun open-cookie-db ()
  (connect (list *cookie-db*)
           :database-type :sqlite3
           :if-exists :old))

(defun find-imdb-cookies ()
  (mapcar (lambda (row)
            (destructuring-bind (domain path name value expires) row
 (make-instance 'drakma:cookie
                :domain  domain
                :path    path
                :name    name
                :value   value
                :expires expires)))
          (select [host_key] [path] [name] [value] [expires_utc]
           :from [cookies]
           :where [= ".imdb.com" [host_key]])) )

(defun obtain-imdb-cookie-jar ()
  (open-cookie-db)
  (make-instance 'drakma:cookie-jar
                 :cookies (find-imdb-cookies)))