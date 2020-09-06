;;;; lecter.asd

(asdf:defsystem #:lecter
    :description "Lecter: reading Delectus 1.x files using libDelectus"
    :author "mikel evins <mikel@evins.net>"
    :license  "Apache 2.0"
    :version "1.5.1d2"
    :serial t
    :components ((:module "src"
                          :serial t
                          :components ((:file "package")
                                       (:file "lecter")))))

;;; (asdf:load-system :lecter)
