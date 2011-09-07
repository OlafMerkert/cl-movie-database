(defpackage :css-select
  (:use :cl :ol-utils)
  (:export :tag-p
           :tag-name
           :tag-attributes :tag-attribute
           :tag-children :tag-child
           :css-selection
           :css-select
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

(defun selector->testfun (selector)
  (with-gensyms!
   (let ((selector (mklist selector)))
     (ecase (length selector)
       ;; Teste nur auf Tag Name
       (1 `(lambda (,g!tag) ,(tag-name-test (first selector) g!tag)))
       ;; Teste auf Tag Name und Vorhandensein des Attributs
       (2 `(lambda (,g!tag)
             (and ,(tag-name-test (first selector) g!tag)
                  (tag-attribute ,g!tag ,(keyw (second selector))))))
       ;; Test auf Tag Name und Inhalt des Attributs
       (3 `(lambda (,g!tag)
             (and ,(tag-name-test (first selector) g!tag)
                  (string-contains
                   ,(third selector)
                   (tag-attribute ,g!tag ,(keyw (second selector)))))))))))

(defun css-selection (selector-stack tag-stream)
  "SELECTOR-STACK ist eine Liste von Funktionen, TAG-STREAM eine Liste von lhtml Knoten."
  (mapcan (lambda (tag)
            (when (tag-p tag)           ; Nur relevant fuer Tags
              (append
               ;; Teste, ob der Selector greift
               (if (funcall (first selector-stack) tag)
                   ;; Falls dies der letzte Selector war,
                   ;; liefere den Tag zurueck
                   (if (rest selector-stack)
                       (css-selection (rest selector-stack) (tag-children tag))
                       (list tag))
                   nil)
               (css-selection selector-stack (tag-children tag))))) 
          tag-stream))

(defmacro css-select (selector &rest nodes)
  "Benutze eine Art von CSS-Selektoren, um einen lhtml-Baum zu durchforsten."
  `(css-selection
    (list ,@(mapcar #'selector->testfun (mklist selector)))
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
