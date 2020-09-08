;;;; cffi.lisp
;;;; cffi wrapper on the Delectus APIs

(in-package #:lecter)

;;; NOTE: works with on macOS with SBCL, Lispworks, and CCL
;;; with ECL, attempting (%init-delectus) signals the error "resource
;;; temporarily unavailable"

;;; ---------------------------------------------------------------------
;;; load and initialize the delectus library
;;; ---------------------------------------------------------------------

(defparameter $libdelectus-path (asdf:system-relative-pathname :lecter "../../libDelectus.dylib"))

(define-foreign-library libdelectus
    (:darwin "libDelectus.dylib")
    (t (:default "libDelectus.dylib")))
   
(defun load-libDelectus ()
  (let ((cffi:*foreign-library-directories*
         (cons $libdelectus-path cffi:*foreign-library-directories*)))
    (use-foreign-library libdelectus)))

;;; ---------------------------------------------------------------------
;;; Delectus library API constants
;;; ---------------------------------------------------------------------

(defparameter $SORT_NONE         0)
(defparameter $SORT_DESCENDING   1)
(defparameter $SORT_ASCENDING    2)
(defparameter $SORT_NUMERIC      3)
(defparameter $SORT_ALPHABETICAL 4)

(defparameter $VAL_NO 0)
(defparameter $VAL_YES 1)
(defparameter $VAL_NO_DOCUMENT 0)
(defparameter $VAL_NO_VALUE 0)
(defparameter $VAL_DEFAULT_VALUE 0)

(defparameter $OBJ_NO_OID 0)

(defparameter $ERR_NO_ERROR            0)
(defparameter $ERR_UNKNOWN_ERROR      -1)
(defparameter $ERR_CANT_CREATE        -2)
(defparameter $ERR_CANT_ADD_ROW       -3)
(defparameter $ERR_CANT_ADD_COLUMN    -4)
(defparameter $ERR_NO_SUCH_COLUMN     -5)
(defparameter $ERR_INDEX_OUT_OF_RANGE -6)
(defparameter $ERR_CANT_UPDATE        -7)
(defparameter $ERR_CANT_WRITE         -8)
(defparameter $ERR_CANT_READ          -9)
(defparameter $ERR_BAD_FORMAT         -10)
(defparameter $ERR_NO_DOCUMENT        -11)
(defparameter $ERR_CANT_RELEASE       -12)


;;; ---------------------------------------------------------------------
;;; Delectus library API functions
;;; ---------------------------------------------------------------------

;;; gambit APIs
;;; -----------

(defcfun ("___release_string" %release-string) :void (s :string))

;;; Delectus APIs
;;; -------------

;;; init the library

(defcfun ("initDelectus" %init-delectus) :int)
(defcfun ("finalizeDelectus" %finalize-delectus) :int)

;;; Delectus APIs

(defcfun ("version" %delectus-version) :string)
(defcfun ("new_delectus" %new-delectus) :int)
(defcfun ("release_delectus" %release-delectus) :int (d :int))
(defcfun ("update_view" %update-view) :int
  (d :int include-deleted :bool sort-column :string sort-order :int filter-text :string))
(defcfun ("count_columns" %count-columns) :int (d :int))
(defcfun ("count_deleted_columns" %count-deleted-columns) :int (d :int))
(defcfun ("column_at_index" %column-at-index) :string (d :int) (index :int))
(defcfun ("sort_column" %sort-column) :string (d :int))
(defcfun ("sort_order" %sort-order) :int (d :int))
(defcfun ("include_deleted" %include-deleted?) :bool (d :int))
(defcfun ("has_deleted" %has-deleted?) :bool (d :int))
(defcfun ("filter_text" %filter-text) :string (d :int))
(defcfun ("count_rows" %count-rows) :int (d :int))
(defcfun ("count_deleted_rows" %count-deleted-rows) :int (d :int))
(defcfun ("value_at" %value-at) :string (d :int)(column-label :string)(row-index :int))
(defcfun ("put_value_at" %put-value-at) :int (d :int)(column-label :string)(row-index :int)(value :string))
(defcfun ("add_row" %add-row) :int (d :int))
(defcfun ("add_column" %add-column) :int (d :int)(label :string))
(defcfun ("rename_column" %rename-column) :int (d :int)(old-label :string)(new-label :string))
(defcfun ("is_column_deleted" %column-deleted?) :bool (d :int)(label :string))
(defcfun ("is_duplicate_label" %duplicate-column-label?) :bool (d :int)(label :string))
(defcfun ("mark_column_deleted" %mark-column-deleted) :int (d :int)(label :string)(deleted? :bool))
(defcfun ("is_row_deleted" %row-deleted?) :bool (d :int)(row-index :int))
(defcfun ("mark_row_deleted" %mark-row-deleted) :int (d :int)(row-index :int)(deleted? :bool))
(defcfun ("compact_delectus" %compact-delectus) :int (d :int))
(defcfun ("write_delectus_file" %write-delectus-file) :int (d :int)(path :string))
(defcfun ("read_delectus_file" %read-delectus-file) :int (path :string))
(defcfun ("write_delectus_csv" %write-delectus-csv) :int (d :int)(path :string))
(defcfun ("read_delectus_csv" %read-delectus-csv) :int (path :string))

;;; (load-libDelectus)
;;; (%init-delectus)
;;; (setf $v (%delectus-version))
;;; (setf $movies-path "/Users/mikel/Workshop/src/delectus/test-data/Movies.delectus")
;;; (setf $id (with-foreign-string (s $movies-path)(%read-delectus-file s)))
;;; (%count-columns $id)
;;; (%count-deleted-columns $id)
;;; (%column-at-index $id 7)
;;; (%sort-column $id)
;;; (%sort-order $id)
;;; (%include-deleted? $id)
;;; (%has-deleted? $id)
;;; (%filter-text $id)
;;; (%count-rows $id)
;;; (%count-deleted-rows $id)
;;; (%value-at $id "Title" 900)
;;; (%column-deleted? $id "Title")
;;; (%duplicate-column-label? $id "Title")
;;; (%row-deleted? $id 2)
