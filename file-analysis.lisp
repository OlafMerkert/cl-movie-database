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

(defparameter *example-dir*
  #P "/media/wdbackup1/filme/")

(defun title-listing (dir)
  (mapcar #'path->title
          (list-directory dir)))

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
  (first (title-search (nth-value 1 (path->titlename path)))))
