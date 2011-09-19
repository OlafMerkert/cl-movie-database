(defpackage :md-retrieval
  (:nicknames :retr)
  (:use :cl :ol-utils :css-select)
  (:export :define-grab :page
           :imdb :name :full :title :year :director :actors :genres
           :tv-series-p :filled-p :episode :season :episodes
           :title-search :title-details :title-episodes
           :fetch ))

(in-package :md-retrieval)

(defparameter *cookie-jar*
  (md-load-cookie:obtain-imdb-cookie-jar))

(defun page-from-url (url &optional parameters)
  (chtml:parse
   (drakma:http-request url :method :get :parameters parameters
                        :cookie-jar *cookie-jar*)
   (chtml:make-lhtml-builder)))

(defmacro define-grab (name params grab-url grab-parameters temporary-vars &body result)
  `(defun ,name (,@params)
     (let ((page (page-from-url
             (format nil ,grab-url ,@params)
             (list ,@(mapcar #`(cons ,(first a1)
                                     ,(second a1))
                             grab-parameters)))))
       (let* ,temporary-vars
         ,@result))))



(defclass imdb ()
  ((name :accessor name
         :initarg  :name
         :initform "")
   (imdb :accessor imdb
         :initarg  :imdb
         :initform "")
   (full :accessor filled-p
         :initarg  :full
         :initform nil)))

(defgeneric fetch (obj))
(defgeneric fetch% (obj))

(defmethod fetch ((imdb imdb))
  (unless (filled-p imdb)
    (fetch% imdb))
  imdb)

(defclass name (imdb)
  ())

(create-standard-print-object imdb  name (imdb))

(defclass title (imdb)
  ((year :accessor year
         :initarg  :year
         :initform nil)
   (director :accessor director
             :initarg  :director
             :initform nil)
   (actors :accessor actors
           :initarg  :actors
           :initform nil)
   (genres :accessor genres
           :initarg  :genres
           :initform nil)))

(defmethod tv-series-p ((title title))
  (and (filled-p title)
       (null (year title))))

(defmethod fetch% ((title title))
  #d
  (let ((other (title-details (imdb title))))
    (dolist (slot '(name year director actors genres))
      (setf (slot-value title slot)
            (slot-value other slot)))
    (setf (slot-value title 'full) t)))

(defmethod episodes ((title title))
  (when (tv-series-p title)
    (let ((ep-list (title-episodes (imdb title))))
      (dolist (x ep-list)
        (setf (slot-value x 'title)
              title))
      ep-list)))

(defclass episode (imdb)
  ((season :accessor season
           :initarg  :season
           :initform 1)
   (episode :accessor episode
            :initarg  :episode
            :initform 1)
   (title :accessor title
          :initarg  :title
          :initform nil)))

(create-standard-print-object episode  ("S" season "E" episode) name)

(define-grab title-search (search-string)
    "http://www.imdb.com/find"
    (("s" "tt")
     ("q" search-string))
    ((page-title (tag-text (css-select1 page (:head) (:title))))
     (nodes (css-select page
                        (:div :id "main")
                        (:table :style nil)
                        #|(:tr)|#
                        #|(child 2)|#
                        (:a :href "^/title/")))
     (imdb (subseq (tag-attribute (css-select1 page (:link :rel "canonical")) :href)
                   #.(length "http://www.imdb.com"))))
  (if (string= page-title "IMDb Title Search") ; make sure we are on the search page
      (mapcar (lambda (node)
                  (make-instance 'title
                                 :name (tag-child node 0)
                                 :imdb (tag-attribute node :href)))
                (remove-if-not (lambda (x) (stringp (tag-child x 0))) nodes))
      ;; otherwise we are already on a detail page
      (list (title-details imdb))))

(defun link->name (a)
  (make-instance 'name :name (tag-text a)
                 :imdb (tag-attribute a :href)))

(define-grab title-details (imdb)
    "http://www.imdb.com~A"
    ()
    ((title-header (css-select1 page (nil :itemprop "name")))
     (name (tag-text title-header))
     (year  (tag-text (css-select1 title-header (:a :href "^/year"))))

     (director(link->name (css-select1 page (nil :itemprop "director"))))

     (actors (mapcar #'link->name (css-select page (nil :itemprop "actors"))))

     (genres (mapcar #'tag-text (css-select page (nil :itemprop "genre")))))
  
  (make-instance 'title 
                 :full t
                 :imdb imdb
                 :name name
                 :year year
                 :director director
                 :actors actors
                 :genres genres))


(define-grab title-episodes (imdb)
    "http://www.imdb.com~Aepisodes"
  ()
  ((episode-nodes (css-select page
                              (:div :class "season-filter-all")
                              (:td)
                              (:h3))))
  (filter (lambda (x)
            (let ((name (css-select1 x :a))
                  (hits (or (not (stringp (tag-text x)))
                            (nth-value 1
                                       (cl-ppcre:scan-to-strings
                                        "Season ([0-9]+), Episode ([0-9]+)"
                                        (tag-text x))))))
              (when hits
                (make-instance 'episode
                               :name (tag-text name)
                               :imdb (tag-attribute name :href)
                               :season (parse-integer (aref hits 0))
                               :episode (parse-integer (aref hits 1))))))
          episode-nodes))
