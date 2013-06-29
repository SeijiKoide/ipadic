(in-package :asdf)  

(defmethod source-file-type ((c cl-source-file) (s module)) "lisp")

(defsystem :ipadic2rdf
    :name "IPAdic to RDF conversion program"
  :author "Seiji Koide <koide@nii.ac.jp>"
  :maintainer "Seiji Koide <koide@nii.ac.jp>"
  :version "2.0.0"
  :licence "MIT"
  :description "Conversion Program from IPAdic to RDF file."
  :long-description "This code is written at Natioanl Institute of Informatics in Japan (NII) for LODAC projects."
  :serial t
  :components
  ((:file "ipadic2rdf")
   )
)

(in-package #:cl-user)

(format t "~%;;To this system, execute the following form:~%~s~%"
  '(asdf:system-load :ipadic2rdf))
