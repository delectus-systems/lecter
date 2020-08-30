#define SORT_NONE         0
#define SORT_DESCENDING   1
#define SORT_ASCENDING    2
#define SORT_NUMERIC      3
#define SORT_ALPHABETICAL 4

#define VAL_NO 0
#define VAL_YES 1
#define VAL_NO_DOCUMENT 0
#define VAL_NO_VALUE 0L
#define VAL_DEFAULT_VALUE 0L

#define OBJ_NO_OID 0L

#define ERR_NO_ERROR            (0)
#define ERR_UNKNOWN_ERROR      (-1)
#define ERR_CANT_CREATE        (-2)
#define ERR_CANT_ADD_ROW       (-3)
#define ERR_CANT_ADD_COLUMN    (-4)
#define ERR_NO_SUCH_COLUMN     (-5)
#define ERR_INDEX_OUT_OF_RANGE (-6)
#define ERR_CANT_UPDATE        (-7)
#define ERR_CANT_WRITE         (-8)
#define ERR_CANT_READ          (-9)
#define ERR_BAD_FORMAT         (-10)
#define ERR_NO_DOCUMENT        (-11)
#define ERR_CANT_RELEASE       (-12)

extern char* version (void);
extern int new_delectus (void);
extern int release_delectus (int);
extern int update_view(int id, bool include_deleted, char* sort_column, int sort_order, char* filter_text);
extern int count_columns (int id);
extern int count_deleted_columns (int id);
extern char* column_at_index (int id, int index);
extern char* sort_column (int id);
extern int sort_order (int id);
extern bool include_deleted (int id);
extern bool has_deleted (int id);
extern char* filter_text (int id);
extern int count_rows (int id);
extern int count_deleted_rows (int id);
extern char* value_at (int id, char* column_label, int row_index); // returns a value id
extern int put_value_at (int id, char* column_label, int row_index, char* value);
extern int add_row (int id);
extern int add_column (int oid, char* label);
extern int rename_column (int oid, char* oldlabel, char* newlabel);
extern bool is_column_deleted (int id, char* column_label);
extern bool is_duplicate_label (int id, char* column_label);
extern int mark_column_deleted (int id, char* column_label, int deleted);
extern bool is_row_deleted (int id, int row_index);
extern int mark_row_deleted (int id, int row_index, bool deleted);
extern int compact_delectus (int id);
extern int write_delectus_file (int id, char* path);
extern int read_delectus_file (char* path);
extern int write_delectus_csv (int id, char* path);
extern int read_delectus_csv (char* path);
