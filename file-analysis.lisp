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

(defun basename (path)
  (or (pathname-name path)
      (last1 (pathname-directory path))))

(defstruct (tname (:type list))
  title-with-year ext path)

(defun path->titlename (path)
  (let* ((dot-parts (split-sequence:split-sequence #\. (basename path)))
         (no-ext (first dot-parts))
         (part (second dot-parts)))
    (list no-ext part path)))

(defun movie-path-listing (path-list)
  (mapcar (lambda (x)
            (cons (first x)
                  (mapcar #'rest (rest x))))
          (group-by #'tname-title (mapcar #'path->titlename path-list))))

(defun genres->dir (genres)
  (let* ((genres (if (<= (length genres) 2) genres
                    (subseq genres 0 2)))
         (genre-dirname (format nil "GENRES/~{~A~^-~}/" genres))
         (genre-dir (merge-pathnames genre-dirname *movie-dir*)))
    (ensure-directories-exist genre-dir)
    genre-dir))

(defun path->title (path)
  (destructuring-bind (title-string &rest files) path
    (let* ((title (fetch (first (title-search title-string))))
           (target-dir (genres->dir (genres title))))
      ;; generate file motions
      (mapcar (lambda (f)
                (let* ((name (format nil "~A~@[ (~A)~]~@[.~A~]"
                                     (name title)
                                     (year title)
                                     (first f)))
                       (target
                        (make-pathname :defaults target-dir
                                       :name name
                                       :type (pathname-type (second f)))))
                  (format t "Moving ~S~%  to ~S.~%"
                          (second f)
                          target)
                  (cons (second f)
                        target)))
              files))))

(defun move-if-confirmed (paths)
  (format t "~&Really move? [y/n] ")
  (when (eq 'y (read))
    (dolist (path paths)
      (destructuring-bind (from . to) path
        (rename-file from to)))))

(defun process-movies ()
  (mapcar (compose #'move-if-confirmed #'path->title)
          (movie-path-listing (movie-file-listing))))

;;; TODO
;;; * better handling of dots/double extensions
;;; * allow refinement of selected title
;;; * including choice of original title (for french/german movies)
;;; * moving directories
;;; * occasional backslashes
;;; * NIL as year
