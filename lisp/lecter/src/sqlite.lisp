;;;; ***********************************************************************
;;;;
;;;; Name:          sqlite.lisp
;;;; Project:       lecter: Delectus 1.x as a library
;;;; Purpose:       sqlite interface
;;;; Author:        mikel evins
;;;; Copyright:     2020 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package #:lecter)

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; function naming conventions:
;;;   db-sqlite-foo: requires a valid, open sqlite handle as an argument
;;;   sqlite-foo: operates on a string or pathname that names a sqlite file;
;;;   calls db-sqlite-foo with with-open-database

;;; GENERIC FUNCTION sqlite-library-version (path)
;;; ---------------------------------------------------------------------
;;; check the SQLite library's version. 

(defun sqlite-library-version ()
  (with-open-database (db ":memory:")
    (execute-single db "SELECT sqlite_version()")))

;;; (sqlite-library-version)

;;; GENERIC FUNCTION sqlite-compile-options (path)
;;; ---------------------------------------------------------------------
;;; check the SQLite library's compile-time options

(defun sqlite-compile-options ()
  (with-open-database (db ":memory:")
    (execute-to-list db "pragma compile_options")))

;;; (sqlite-compile-options)

;;; GENERIC FUNCTION valid-sqlite-file? (path)
;;; ---------------------------------------------------------------------
;;; returns PATH if it's a valid SQLite file; returns NIL if it isn't
;;; a valid sqlite-file database is an existing SQLite file that
;;; we can read from

(defmethod valid-sqlite-file? ((path pathname))
  (let ((path (probe-file path)))
    (and path
         (uiop:file-pathname-p path)
         (handler-case (with-open-database (db path)
                         ;; the right way to check whether a file is a SQLite file,
                         ;; according to SQLite docs:
                         (execute-non-query db "pragma schema_version"))
           (condition (c)
             (declare (ignore c))
             nil))
         path)))

(defmethod valid-sqlite-file? ((path string))
  (valid-sqlite-file? (pathname path)))

;;; tests:
;;; (defparameter $testpath (uiop:native-namestring "~/.emacs"))
;;; (valid-sqlite-file? $testpath)
;;; (defparameter $testpath (uiop:native-namestring "~//Workshop/data/kinder/kinder_data.sqlite3"))
;;; (valid-sqlite-file? $testpath)


;;; GENERIC FUNCTION sqlite-list-tables (path)
;;; ---------------------------------------------------------------------
;;; returns a list of table names from the file at PATH

(defmethod sqlite-list-tables ((path pathname))
  (sqlite:with-open-database (db path)
    (mapcar #'car (sqlite:execute-to-list db "SELECT name FROM sqlite_master WHERE type = \"table\""))))

(defmethod sqlite-list-tables ((path string))
  (sqlite-list-tables (pathname path)))

;;; (sqlite-list-tables $testpath)

;;; GENERIC FUNCTION sqlite-list-table-column-info (path table-name)
;;; ---------------------------------------------------------------------
;;; returns a list of column descriptions from the named table in the
;;; file at PATH

(defmethod db-sqlite-table-column-info ((db sqlite-handle) (table-name string))
  (sqlite:execute-to-list db (format nil "pragma table_info(~S)" table-name)))

(defmethod sqlite-table-column-info ((path pathname) (table-name string))
  (sqlite:with-open-database (db path)
    (db-sqlite-table-column-info db table-name)))

(defmethod sqlite-table-column-info ((path string) (table-name string))
  (sqlite-table-column-info (pathname path) table-name))

;;; (sqlite-table-column-info $testpath "kinder_data")

