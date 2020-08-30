;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          delectus.scm
;;;; Project:       Delectus
;;;; Purpose:       the C interface to the Delectus API
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(c-define (c:version) () 
          char-string "version" ""
          (api:version))

(c-define (c:new-delectus) () 
          int "new_delectus" ""
          (api:new-delectus))

(c-define (c:release-delectus id) (int) 
          int "release_delectus" ""
          (api:release-delectus id))

(c-define (c:update-view! id include-deleted? sort-column sort-order filter-text) 
          (int bool char-string int char-string)
          int "update_view" ""
          (api:update-view! id include-deleted? sort-column sort-order filter-text))

(c-define (c:count-rows id) (int) 
          int "count_rows" ""
          (api:count-rows id))

(c-define (c:count-deleted-rows id) (int) 
          int "count_deleted_rows" ""
          (api:count-deleted-rows id))

(c-define (c:count-columns id) (int) 
          int "count_columns" ""
          (api:count-columns id))

(c-define (c:count-deleted-columns id) (int) 
          int "count_deleted_columns" ""
          (api:count-deleted-columns id))

(c-define (c:column-at-index id i) (int int) 
          char-string "column_at_index" ""
          (api:column-at-index id i))

(c-define (c:sort-column id) (int) 
          char-string "sort_column" ""
          (api:sort-column id))

(c-define (c:sort-order id) (int) 
          int "sort_order" ""
          (api:sort-order id))

(c-define (c:include-deleted? id) (int) 
          bool "include_deleted" ""
          (api:include-deleted? id))

(c-define (c:has-deleted? id) (int) 
          bool "has_deleted" ""
          (api:has-deleted? id))

(c-define (c:filter-text id) (int) 
          char-string "filter_text" ""
          (api:filter-text id))

(c-define (c:value-at id column-label row-index) (int char-string int) 
          char-string "value_at" ""
          (api:value-at id column-label row-index))

(c-define (c:put-value-at id column-label row-index val) (int char-string int char-string) 
          int "put_value_at" ""
          (api:put-value-at! id column-label row-index val))

(c-define (c:add-row! id) (int) 
          int "add_row" ""
          (api:add-row! id))

(c-define (c:add-column! id column-label) (int char-string) 
          int "add_column" ""
          (api:add-column! id column-label))

(c-define (c:rename-column! id old-label new-label) (int char-string char-string) 
          int "rename_column" ""
          (api:rename-column! id old-label new-label))

(c-define (c:column-deleted? id column-label) (int char-string)
          bool "is_column_deleted" ""
          (api:column-deleted? id column-label))

(c-define (c:mark-column-deleted! id column-label deleted?) (int char-string bool)
          int "mark_column_deleted" ""
          (api:mark-column-deleted! id column-label deleted?))

(c-define (c:duplicate-label? id column-label) (int char-string)
          bool "is_duplicate_label" ""
          (api:duplicate-label? id column-label))

(c-define (c:row-deleted? id row-index) (int int)
          bool "is_row_deleted" ""
          (api:row-deleted? id row-index))

(c-define (c:mark-row-deleted! id row-index deleted?) (int int bool)
          int "mark_row_deleted" ""
          (api:mark-row-deleted! id row-index deleted?))

(c-define (c:compact-delectus! id)(int)
          int "compact_delectus" ""
          (api:compact-delectus! id))

(c-define (c:write-delectus-file id path)(int char-string)
          int "write_delectus_file" ""
          (api:write-delectus-file id path))

(c-define (c:read-delectus-file path)(char-string)
          int "read_delectus_file" ""
          (api:read-delectus-file path))

(c-define (c:write-delectus-csv id path)(int char-string)
          int "write_delectus_csv" ""
          (api:write-delectus-csv id path))

(c-define (c:read-delectus-csv path)(char-string)
          int "read_delectus_csv" ""
          (api:read-delectus-csv path))


