(defpackage :css-select
  (:use :cl :ol-utils)
  (:export :tag-p
           :tag-name
           :tag-attributes :tag-attribute
           :tag-children :tag-child
           :css-selection
           :css-select
           :fun :next :previous
           :tag :parent :child
           :strip
           :string-contains))

(in-package :css-select)


;; lhtml interface
(defun tag-p (tag)
  "Ein lhtml Tag ist eine Liste, die mit einem Symbol beginnt."
  (and (consp tag)
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

(defmacro! in (o!x &rest possibilities)
  `(or
    ,@(mapcar #`(eq ,g!x ,a1) possibilities)))

(defun special-selector-p (selector)
  (in (car selector) 'fun 'parent 'next 'previous 'child))

(defun selector->testfun (selector)
  (with-gensyms!
    (let ((selector (mklist selector)))
      (cond
        ((eq (car selector) 'fun)       ; special function evaluation
         `(ilambda (tag parent)
            ,@(cdr selector)))
        ((eq (car selector) 'parent)
         `(ilambda (tag parent)
            parent))
        ((eq (car selector) 'next)
         `(ilambda (tag parent)
            (tag-child parent
                       (+ (position tag (tag-children parent)) 1))))
        ((eq (car selector) 'previous)
         `(ilambda (tag parent)
            (tag-child parent
                       (- (position tag (tag-children parent)) 1))))
        ((eq (car selector) 'child)
         `(ilambda (tag parent)
            (tag-child tag ,(second selector))))
        (t                              ; Standard Css selector
         (ecase (length selector)
           ;; Teste nur auf Tag Name
           (1 `(ilambda (,g!tag ,g!parent) ,(tag-name-test (first selector) g!tag)))
           ;; Teste auf Tag Name und Vorhandensein des Attributs
           (2 `(ilambda (,g!tag ,g!parent)
                 (and ,(tag-name-test (first selector) g!tag)
                      (tag-attribute ,g!tag ,(keyw (second selector))))))
           ;; Test auf Tag Name und Inhalt des Attributs/ bzw Attribut nicht vorhanden!
           (3 `(ilambda (,g!tag ,g!parent)
                 (and ,(tag-name-test (first selector) g!tag)
                      ,(if (eq (third selector) nil)
                           ;; Attribut soll nicht vorhanden sein!
                           `(not (tag-attribute ,g!tag ,(keyw (second selector))))
                           ;; Teste Attribut auf regex
                           `(string-contains
                             ,(third selector)
                             (tag-attribute ,g!tag ,(keyw (second selector))))))))))))))

(defun css-selection (selector-stack special-stack tag-list &optional parent)
  "SELECTOR-STACK ist eine Liste von Funktionen, TAG-LIST eine Liste von lhtml Knoten."
  #d
  ;; Noch weitere Selektoren
  (let* ((tag (first tag-list))
         (tagp (tag-p tag)))
    (cond ((null selector-stack)    ; no more selectors -- return data
           (if parent
               (list parent)
               (copy-list tag-list)))
          ((null tag-list) nil)      ; no tags -- nothing to do
          ((first special-stack) ; filter through the special selector
           (css-selection (rest selector-stack) (rest special-stack)
                          (filter (lambda (x) (funcall (first selector-stack) x parent))
                                  tag-list)))
          ((and tagp
                (funcall (first selector-stack) tag parent))
           ;; ^^ the first item is actually a tag and matches
           (nconc
            (css-selection (rest selector-stack) (rest special-stack)
                           (tag-children tag)
                           tag)
            #1=(css-selection selector-stack special-stack (rest tag-list) parent)))
          (tagp            ; first item is a tag, doesn't match though
           (nconc
            (css-selection selector-stack special-stack
                           (tag-children tag)
                           tag)
            #1#))
          (t                            ; first item not a tag
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
