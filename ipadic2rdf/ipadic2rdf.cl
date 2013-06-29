;;;-*- Mode: common-lisp; syntax: common-lisp; package: ipa; base: 10 -*-
;;;
;;; Transformation Program from IPADIC to RDF
;;;
;;; Programed by Seiji Koide at National Institute of Informatics
;;; Copyright, CC-by-SA

(defpackage :ipa)
(in-package :ipa)

(eval-when (:load-toplevel :execute)
  (defparameter *ipadic-directory*
    (make-pathname :host (pathname-host *load-truename*)
                   :device (pathname-device *load-truename*)
                   :directory #+(or :unix :linux) '(:absolute "usr" "share" "chasen" "dic" "ipadic-2.7.0-utf8")
                              #+:mswindows '(:absolute "ipadic-sjis-2.7.0")))
  ) ; end of eval-when

(defvar *noun-word-list* nil)
(defvar *number-of-noun-words* 197489)

;;;
;;; Directory functions
;;;
;;; These utilities are from "Practical Common Lisp" by Peter Seibel
;;;

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (p)
  (and (not (component-present-p (pathname-name p)))
       (not (component-present-p (pathname-type p)))
       p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
        (make-pathname
         :directory (append (or (pathname-directory pathname) (list :relative))
                            (list (file-namestring pathname)))
         :name nil
         :type nil
         :defaults pathname)
      pathname)))

(defun directory-wildcard (dirname)
  (make-pathname
   :name :wild
   :type #-clisp :wild #+clisp nil
   :defaults (pathname-as-directory dirname)))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  (directory (directory-wildcard dirname)))

;;;
;;; This is taken from Util.cl of RDF
;;;

(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking <str> at every occurence
of <ch> (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
          (next-pos (position ch str)
                    (position ch str :start prev-pos))
          (stuff (list (subseq str 0 next-pos))
                 (cons (subseq str prev-pos next-pos)
                       stuff)))
         ((null next-pos) (nreverse stuff)))))

;;;
;;; The following is copied from SWCLOS.
;;;

(defun iri-escape-for-symbol-name (symbol-name)
  "<symbol-name> is a string of symbol. It turns out to iri fragment or a tail of path in IRI. So, it must be
   escaped for gen-delims characters except #\: and #\@. In this version, a space is also escaped."
  (cond ((and (> (length symbol-name) 5) (string= "http:" (subseq symbol-name 0 5)))
         symbol-name) ; this is for ontology URIs
        (t (flet ((escape-p (c)
                            (declare (optimize (speed 3) (safety 1)))
                            (or (char= c #\/)
                                (char= c #\?)
                                (char= c #\#)
                                (char= c #\[)
                                (char= c #\])
                                (eq (char-code c) #x20))))
             (labels ((escape (str)
                              (let ((pos 0))
                                (cond ((setq pos (position-if #'escape-p str)) ; found
                                       (let ((c (char str pos)))
                                         (concatenate 'cl:string
                                           (subseq str 0 pos)
                                           (format nil "%~X" (char-code c))
                                           (escape (subseq str (1+ pos))))))
                                      (t str)))))
               (escape symbol-name))))))

;;;
;;;
;;;
#+:mswindows
(defconstant *xml-decl* "<?xml version='1.0' encoding='Shift_JIS'?>~%")
#-:mswindows
(defconstant *xml-decl* "<?xml version='1.0' encoding='UTF-8'?>~%")

(defparameter *doc-decl* "<!DOCTYPE rdf:RDF [
    <!ENTITY rdf 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'>
    <!ENTITY rdfs 'http://www.w3.org/2000/01/rdf-schema#'>
    <!ENTITY ipadic27instances 'http://www.ipadic.jp/270/instances/'>
    <!ENTITY ipadic27schema 'http://www.ipadic.jp/270/schema#'>
]>~%~%")
(defparameter *rdf-decl-prolog*
"<rdf:RDF
    xmlns:rdf=\"&rdf;\"
    xmlns:rdfs=\"&rdfs;\"
    xmlns:ipadic27instances=\"&ipadic27instances;\"
    xmlns:ipadic27schema=\"&ipadic27schema;\"
    xml:lang=\"en\">~%")
(defconstant *rdf-decl-epilog* "</rdf:RDF>~%")


(defun list-dic-files ()
  (directory 
   (make-pathname
    :name :wild
    :type "dic"
    :defaults (pathname-as-directory *ipadic-directory*))))

(defun noun-dictionary-p (pathname)
  (string= "Noun" (first (split-seq-on (pathname-name pathname) #\.))))

(defun list-noun-dic-files ()
  (let ((dic-pathnames
         (directory 
          (make-pathname
           :name :wild
           :type "dic"
           :defaults (pathname-as-directory *ipadic-directory*)))))
    (loop for dicp in dic-pathnames
        when (noun-dictionary-p dicp)
        collect dicp)))

;;;
;;;
;;;


(defstruct (ïiéå (:type list) :named (:conc-name hinshi-))
  data)

(defstruct (ïiéå-data (:type list) (:conc-name hinshi-data-))
  type subtype category optional)

(defun hinshi-type (hinshi)
  (hinshi-data-type (hinshi-data hinshi)))

(defun hinshi-subtype (hinshi)
  (hinshi-data-subtype (hinshi-data hinshi)))

(defun hinshi-category (hinshi)
  (hinshi-data-category (hinshi-data hinshi)))

(defun hinshi-optional (hinshi)
  (hinshi-data-optional (hinshi-data hinshi)))

(defstruct (åÍ (:type list) (:conc-name word-))
  lemma reading pronunciation)

(defstruct (å©èoÇµåÍ (:type list) :named (:conc-name lemma-))
  data)

(defstruct (word-lemma-data (:type list))
  name cost)

(defstruct (ì«Ç› (:type list) :named (:conc-name yomi-))
  data)

(defstruct (î≠âπ (:type list) :named (:conc-name hatuon-))
  data)

(defun word-name (word)
  (word-lemma-data-name (lemma-data (word-lemma word))))

(defun word-cost (word)
  (word-lemma-data-cost (lemma-data (word-lemma word))))

(defun word-yomi (word)
  (yomi-data (word-reading word)))

(defun word-hatuon (word)
  (hatuon-data (word-pronunciation word)))

;;;
;;;
;;;

(defun ipadic2rdf ()
  (setq *noun-word-list* nil)
  (loop for noun-file in (list-noun-dic-files)
      do (with-open-file (ipastream noun-file)
           (let ((line-pos 0))
             (loop for line = (read-line ipastream nil nil)
                 while line
                 do (multiple-value-bind (hinshi pos) (read-from-string line nil nil)
                      (let ((word (read-from-string line nil nil :start pos)))
                        (assert (string= "ñºéå" (hinshi-type hinshi)))
                        (let ((prev (get (word-name word) :ipa))
                              (this (format nil "~A#~D" (pathname-name noun-file) line-pos)))
                          (cond ((null prev)
                                 (setf (get (word-name word) :ipa) this)
                                 (push (word-name word) *noun-word-list*))
                                ((atom prev)
                                 (when (not (eq prev this))
                                   (setf (get (word-name word) :ipa) (list this prev)))
                                 ;; otherwise nothing done
                                 )
                                (t ; then list
                                 (when (not (member this prev))
                                   (setf (get (word-name word) :ipa) (cons this prev))))))
                        ))
                   (setq line-pos (file-position ipastream))))))
  (setq *number-of-noun-words* (length *noun-word-list*))
  (setq *noun-word-list* (sort *noun-word-list* #'string<))
  (assert (= *number-of-noun-words* (length *noun-word-list*)))
  (format t "~%Number of noun words: ~D~%" (length *noun-word-list*))
  (let ((rdffile (make-pathname :name "ipadic" :type "rdf" :defaults (user-homedir-pathname))))
    (flet ((%ipadic2rdf (rdfstream word)
               (let ((escaped-word-string (iri-escape-for-symbol-name (symbol-name word))))
                 (format rdfstream "<ipadic27schema:Word rdf:about=\"&ipadic27instances;word-~A\">~%" escaped-word-string)
                 (format rdfstream "  <ipa27schema:lexicalForm>\"~A\"@ja</ipa27schema:lexicalForm>~%" word)
                 (let ((file&line-poses
                        (cond ((not (consp (get word :ipa))) (list (get word :ipa)))
                              (t (get word :ipa)))))
                   (loop for file&line-pos in file&line-poses with n = 0
                       do (incf n)
                         (format rdfstream "  <ipa27schema:sense rdf:resource=\"&ipadic27instances;wordsense-~A-noun-~D\"/>~%" escaped-word-string n))
                   (format rdfstream "</ipadic27schema:Word>~%")
                   (format rdfstream "~%")
                   (loop for file&line-pos in file&line-poses with n = 0
                       do (incf n)
                         (format rdfstream "<ipadic27schema:NounWordSense rdf:about=\"&ipadic27instances;wordsense-~A-noun-~D\">~%" escaped-word-string n)
                         (format rdfstream "  <rdfs:label>\"~A\"@ja</rdfs:label>~%" word)
                         (setq file&line-pos (split-seq-on file&line-pos #\#))
                         (with-open-file (stream (make-pathname
                                                  :name (first file&line-pos)
                                                  :type "dic"
                                                  :defaults *ipadic-directory*))
                           (file-position stream (parse-integer (second file&line-pos)))
                           (let* ((hinshidata (read stream))
                                  (worddata (read stream)))
                             (assert (string= word (word-name worddata)))
                             (format rdfstream "  <ipadic27schema:yomi>\"~A\"@ja</ipadic27schema:yomi>~%" (word-yomi worddata))
                             (format rdfstream "  <ipadic27schema:hatuon>\"~S\"@ja</ipadic27schema:hatuon>~%" (word-hatuon worddata))
                             (format rdfstream "  <ipadic27schema:cost>~D</ipadic27schema:cost>~%" (word-cost worddata))
                             (format rdfstream "  <ipadic27schema:subpos rdf:resource=\"&ipadic27schema;Noun-~A\"/>~%" (hinshi-subtype hinshidata))
                             (when (hinshi-category hinshidata)
                               (format rdfstream "  <ipadic27schema:category rdf:resource=\"&ipadic27schema;Noun-~A\"/>~%" (hinshi-category hinshidata)))
                             (when (hinshi-optional hinshidata)
                               (format rdfstream "  <ipadic27schema:optional rdf:resource=\"&ipadic27schema;Noun-~A\"/>~%" (hinshi-optional hinshidata)))))
                         (format rdfstream "</ipadic27schema:NounWordSense>~%")
                         (format rdfstream "~%")))
                 )))
      (with-open-file (rdfstream rdffile :direction :output :if-exists :supersede)
        (format rdfstream *xml-decl*)
        (format rdfstream *doc-decl*)
        (format rdfstream *rdf-decl-prolog*)
        (loop for word in *noun-word-list*
            do (%ipadic2rdf rdfstream word))
        (format rdfstream *rdf-decl-epilog*)
        ))))