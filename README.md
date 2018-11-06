# cworks
functions for querying student info from downloaded CourseWorks files

This package is specifically tailored to the assignment names I use in CourseWorks.

It works with files that are downloaded from CourseWorks using the "Export" button on the grades page.

Everytime new versions of the files are downloaded, call `create_NEW()` to tidy up the data and write to new files (same names with NEW prefix added.)

At the beginning of every session, get the student data as follows:

`df <- get_student_data()` 

and then pass it to other functions as desired:

`hmk(name, num, df)`  Homework score for homework # `num

`lpr(name, df)` Late passes remaining

`missed_test(num, df)`  Missed an exam

`name2uni(name, df)`  Convert name to UNI

`tst(name, num, df)`  Test score for test # `num`

`uni2name(UNI, df)`   Convert UNI to name

`wc(name, df)` Which class?
