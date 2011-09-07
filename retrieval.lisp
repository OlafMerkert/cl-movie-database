(defpackage :md-retrieval
  (:nicknames :retr)
  (:use :cl :ol-utils :css-select)
  (:export))

(in-package :md-retrieval)

(defmacro define-grab (name params grab-url &rest expressions))

(define-grab title-search (search-string)
    "http://www.imdb.com/find?s=tt&q=~A"
    ())

(defparameter *example-url*
  (format nil "http://www.imdb.com/find?s=tt&q=~A"
          "maxe"))

(defparameter *example-page*
  (chtml:parse (drakma:http-request *example-url*)
               (chtml:make-lhtml-builder)))

(defun tryout-expression ()
  (third
   (css-select:css-select (:tr :td)
          (second
           (css-select:css-select
            ((:div :id "main") (:table))
            *example-page*)))))