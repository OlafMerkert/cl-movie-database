(defpackage :md-retrieval
  (:nicknames :retr)
  (:use :cl :ol-utils :css-select)
  (:export))

(in-package :md-retrieval)

(defun page-from-url (url)
  (chtml:parse
   (drakma:http-request url)
   (chtml:make-lhtml-builder)))

(defmacro define-grab (name params grab-url temporary-vars &body result)
  `(defun ,name ,params
     (let ((page (page-from-url
                  (format nil ,grab-url ,@params))))
       (let* ,temporary-vars
         ,@result))))

(defmacro! define-standard-print-object
    (class &rest slots)
  (let ((slots-flat (flatten slots)))
    `(defmethod print-object ((,g!object ,class) ,g!stream)
       (print-unreadable-object (,g!object ,g!stream :type t)
         (with-slots ,slots-flat ,g!object
           (format ,g!stream ,(format nil "~{~A~^ ~}"
                                      (mapcar (lambda (x)
                                                (if (listp x)
                                                    "[~A]" "~A"))
                                              slots))
                   ,@slots-flat))))))

(defclass name ()
  ((name :accessor name
         :initarg  :name
         :initform "")
   (imdb :accessor imdb
         :initarg  :imdb
         :initform "")))

(define-standard-print-object name  name (imdb))

(defclass title ()
  ((name :accessor name
         :initarg  :name
         :initform "")
   (imdb :accessor imdb
         :initarg  :imdb
         :initform "")
   (year)
   (original-name)))

(define-standard-print-object title  name (imdb))

(defclass episode ()
  ((season :accessor season
           :initarg  :season
           :initform 1)
   (episode :accessor episode
            :initarg  :episode
            :initform 1)
   (name :accessor name
         :initarg  :name
         :initform "")
   (title :accessor title
          :initarg  :title
          :initform nil)))

(define-standard-print-object episode  season episode (name))

(define-grab title-search (search-string)
    "http://www.imdb.com/find?s=tt&q=~A"
    ((nodes (css-select page
                        (:div :id "main")
                        (:table :style nil)
                        (:tr)
                        (child 2)
                        (:a))))
  (mapcar (lambda (node)
            (make-instance 'title
                           :name (tag-child node 0)
                           :imdb (tag-attribute node :href)))
          nodes))

(defun link->name (a)
  (make-instance 'name :name (tag-text a)
                 :imdb (tag-attribute a :href)))

(define-grab title-details (imdb)
    "http://www.imdb.com~A"
    ((title-header (css-select1 page (nil :itemprop "name")))
     (name (tag-text title-header))
     (year  (tag-text (css-select1 title-header (:a :href "^/year"))))

     (director(link->name (css-select1 page (nil :itemprop "director"))))

     (actors (mapcar #'link->name (css-select page (nil :itemprop "actors"))))

     (genres (mapcar #'tag-text (css-select page (nil :itemprop "genre")))))
  
  (list 'title 
        :full t
        :imdb imdb
        :name name
        :year year
        :director director
        :actors actors
        :genres genres))


(define-grab title-episodes (imdb)
    "http://www.imdb.com~Aepisodes"
    ((episode-nodes (css-select page
                                (:div :class "season-filter-all")
                                (:td)
                                (:h3))))
  (filter (lambda (x)
            (let ((name (tag-text (css-select1 x :a)))
                  (hits (or (not (stringp (tag-text x)))
                            (nth-value 1
                                       (cl-ppcre:scan-to-strings
                                        "Season ([0-9]+), Episode ([0-9]+)"
                                        (tag-text x))))))
              (when hits
                (make-instance 'episode
                               :name name
                               :season (parse-integer (aref hits 0))
                               :episode (parse-integer (aref hits 1))))))
          episode-nodes))
