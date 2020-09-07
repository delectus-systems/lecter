;;;; lecter.asd

(asdf:defsystem #:lecter
    :description "Lecter: reading Delectus 1.x files using libDelectus"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2.0"
    :version "1.5.2"
    :serial t
    :depends-on (:cffi)
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       #+sbcl (:file "sbcl")
                                       #+lispworks (:file "lispworks")))))

;;; (asdf:load-system :lecter)
