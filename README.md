# lecter

The Delectus 1.x data-storage engine built as a shared library named **libdelectus** using Gambit Scheme, and a command tool named **lecter** that uses the library to read documents in Delectus 1.x format.

**lecter** can read all Delectus 1.x file formats and convert them to any of the following text formats:

- Lisp (sexp): as Lisp-style s-expressions
- delectus-jsonl: as a sequence of JSON values, one per line. The sequence obeys the following rules:
  - The first line is a JSON object that gives the file format, created time, and modified time of the document
  - The second line is a JSON object the gives the column metadata for the document. THe format is an object of the form `{"columns": [...]}` where each element of the array value is a JSON object of the form `{"label": L, "deleted": D}`. `L` is the a string that gives the label of the column; `D` is true if the column is marked deleted and false if not.
  - The and subsequent lines are JSON arrays. Each line gives the value for each field of one item in the document. Each element of the array is the value for the corresponding column given in the "column" array.
- jsonl: as JSON objects, one object per line

## Building and using

`make` or `make exe` to build `lecter`, a command-line tool for
reading and converting Delectus v1.x data files. Run `lecter` with no
argument for help using it.

`make lib` to build `libDelectus.a`. Use by statically linking to an application project.

`make dylib` to build `libDelectus.dylib`, the Delectus data engine as a dynamically-loadable library. Use by loading the library, calling initDelectus(), and then using the C functions declared in `include/Delectus.h`.

test-data contains several files in Delectus v 1.x and other formats
suitable for use with libDelectus and applications built with it.

`lisp/lecter` contains a small Common Lisp project that can exercise
libDelectus by loading the dylib and calling its APIs.

## lecter usage

  **`lecter --version`**  prints the version of lecter
  **`lecter --uuid`**  prints a newly-generated v4 UUID
  **`lecter --format-version PATH`**  prints the version number of the delectus file format,
                                or INVALID if it's not a recognized Delectus format
  **`lecter --format-name PATH`**  prints the version name of the delectus file format,
                             or INVALID if it's not a recognized Delectus format
  **`lecter --lisp PATH`**  prints the Delectus data to stdio as Common Lisp expressions
  **`lecter --sexp PATH`**  prints the Delectus data to stdio as Common Lisp expressions
  **`lecter --delectus-json PATH`**  prints the Delectus data to stdio as JSON, colums followed by rows
  **`lecter --jsonl PATH`**  prints the Delectus data to stdio as JSON objects, one object per line
  **`lecter --csv PATH`**  prints the Delectus data to stdio as CSV
