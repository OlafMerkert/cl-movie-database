(defpackage :css-select
  (:use :cl :ol-utils)
  (:export :tag-p
           :tag-name
           :tag-attributes :tag-attribute
           :tag-children :tag-child
           :css-selection
           :css-select
           :tag :parent
           :strip
           :string-contains))

(in-package :css-select)


;; lhtml interface
(defun tag-p (tag)
  "Ein lhtml Tag ist eine Liste, die mit einem Symbol beginnt."
  (and (listp tag)
       (symbolp (first tag))))

(defun tag-name (tag)
  "Dieses Symbol ist der Tagname, normalerweise ein :keyword."
  (first tag))

(defun tag-attributes (tag)
  "Die Attribute bilden eine alist."
  (second tag))

(defun tag-attribute (tag attr)
  "Die man nach konkreten Attributen durchsuchen kann."
  (assoc1a attr (second tag)))

(defun tag-children (tag)
  "Die Kindelemente sind wieder eine Liste."
  (cddr tag))

(defun tag-child (tag n)
  "Die man auch mit Indizes durchsuchen kann."
  (nth (+ 2 n) tag))


(defun tag-name-test (s tag)
  "Erzeuge Code, der den Tag-Namen von TAG auf Gleichheit mit"
  (if s
      `(eq (tag-name ,tag)
                  ,(keyw s))
      t))

(defun special-selector-p (selector)
  (eq 'fun (car selector)))

(defun selector->testfun (selector)
  (with-gensyms!
   (let ((selector (mklist selector)))
     (cond
       ((special-selector-p selector)   ; special function evaluation
        `(ilambda (tag parent)
           ,@(cdr selector)))
       (t                               ; Standard Css selector
        (ecase (length selector)
          ;; Teste nur auf Tag Name
          (1 `(ilambda (,g!tag ,g!parent) ,(tag-name-test (first selector) g!tag)))
          ;; Teste auf Tag Name und Vorhandensein des Attributs
          (2 `(ilambda (,g!tag ,g!parent)
                (and ,(tag-name-test (first selector) g!tag)
                     (tag-attribute ,g!tag ,(keyw (second selector))))))
          ;; Test auf Tag Name und Inhalt des Attributs
          (3 `(ilambda (,g!tag ,g!parent)
                (and ,(tag-name-test (first selector) g!tag)
                     (string-contains
                      ,(third selector)
                      (tag-attribute ,g!tag ,(keyw (second selector)))))))))))))

(defun css-selection (selector-stack special-stack tag-list &optional parent)
  "SELECTOR-STACK ist eine Liste von Funktionen, TAG-LIST eine Liste von lhtml Knoten."
  ;; Noch weitere Selektoren
  (cond ((null selector-stack)      ; no more selectors -- return data
         (if parent
             (list parent)
             (copy-list tag-list)))
        ((null tag-list))      ; no tags -- nothing to do
        ((first special-stack)   ; filter through the special selector
         (css-selection (rest selector-stack) (rest special-stack)
                        (filter (lambda (x) (funcall (first selector-stack) x parent))
                                tag-list)))
        ((funcall (first selector-stack) (first tag-list) parent)
         ;; ^^ the first item matches
         (nconc
          (css-selection (rest selector-stack) (rest special-stack)
                         (tag-children (first tag-list))
                         (first tag-list))
          #1=(css-selection selector-stack special-stack (rest tag-list) parent)))
        (t                              ; no match, go deeper
         (nconc
          (css-selection selector-stack special-stack
                         (tag-children (first tag-list))
                         (first tag-list))
          #1#))))

(defmacro css-select (selector &rest nodes)
  "Benutze eine Art von CSS-Selektoren, um einen lhtml-Baum zu durchforsten."
  `(css-selection
    (list ,@(mapcar #'selector->testfun (mklist selector)))
    (list ,@(mapcar #'special-selector-p (mklist selector)))
    (list ,@nodes)))

;; Bsp
;; (css-select p *sample*) ; Alle p -Tags

;; (css-select (p a) *sample*) ; Alle a -Tags, die ein p -Tag als Vorfahr haben

;; Alle a -Tags, die ein href Attribut haben und ein p -Tag als Vorfahr
;; (css-select (p (a href)) *sample*)

;; alle p -Tags, die class Attribut = footer haben
;; (css-select ((p class "footer")) *sample*)

(defun strip (str)
  "Remove all whitespace from the beginning and end of STR."
  (values (cl-ppcre:regex-replace-all "^\\s+|\\s+$" str "")))

(defun string-contains (sub super)
  "Check, whether the regex pattern SUB matches the string SUPER."
  #|(search sub super :test #'char=)|#
  (values (cl-ppcre:scan sub super)))
