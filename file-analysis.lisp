(defpackage :md-file-analysis
  (:use :cl :ol-utils :md-retrieval
        :com.gigamonkeys.pathnames)
  (:export))

(in-package :md-file-analysis)

(defclass file-object ()
  ((basename)
   (ext)
   (imdb-title)
   ))

(defparameter *movie-dir*
  #P "/media/wdbackup1/filme/")

(defparameter *blacklist*
  (mapcar
   (lambda (p) (merge-pathnames p *movie-dir*))
   '("SERIEN/"
     "GENRES/"
     "MIT Lisp Lectures/"
     "Operation Red Flag/"
     "Star Trek/"
     "Woody Allen/")))

(defun movie-file-listing ()
  (sort 
   (set-difference (list-directory *movie-dir*)
                   *blacklist*
                   :test #'equal)
   #'string-lessp :key #'basename))

(defun title-listing (dir)
  (mapcar #'path->title
          (movie-file-listing)))

(defun basename (path)
  (or (pathname-name path)
      (last1 (pathname-directory path))))

(defun path->titlename (path)
  (let* ((dot-parts (split-sequence:split-sequence #\. (basename path)))
         (no-ext (first dot-parts))
         (part (second dot-parts))
         (has-year (cl-ppcre:scan " \\([0-9]+\\)" no-ext)))
    (if has-year
        (values (subseq no-ext 0 has-year)
                no-ext
                part)
        (values no-ext no-ext part))))

(defun path->title (path)
  (fetch (first (title-search (nth-value 1 (path->titlename path))))))
