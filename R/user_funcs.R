############### create_NEW() ###############

#' Create new student info files, one per class
#'
#' @description
#' Clears previous NEW files, creates new ones from downloaded CourseWorks files, and writes to the path specified, prefixing the original filename with "NEW". Nothing is returned.
#'
#' Call only if new files have been downloaded from CourseWorks
#'
#' @param path folder where student files are stored
#'
#' @param pattern pattern to match in filenames
#'
#' @export
#'
#' @examples
#' create_NEW()
#' create_NEW("~/Downloads", "Spring_2019")

create_NEW <- function(path = "~/Documents/Students",
                       pattern = "Fall_2018") {

  # get rid of previous NEW files

  newfiles <- file.path(path, dir(path, pattern = "NEW"))

  if (length(newfiles) > 0) purrr::map(newfiles, file.remove)

  # get current files

  files <- dir(path, pattern)

  # create NEW files
  purrr::map(files, fixfiles, path = path)

}

############### set_student_data() ###############

#' Get student info in tidy format
#'
#' @description
#'
#' Reads in all files with NEW prefix and combines into into one tidy `tibble`.
#'
#' @inheritParams create_NEW
#'
#' @return a tidy data frame with student info
#'
#' @export
#'
#' @examples
#' stud_df <- set_student_data()
#' stud_df <- set_student_data("~/Downloads")

set_student_data <- function(path = "~/Documents/Students", pattern = "NEW") {

  files <- dir(path, pattern)

  assign("studenv", new.env(), envir = .GlobalEnv)

  studenv$studdata <- suppressMessages(purrr::map_df(files, ~readr::read_csv(file.path(path, .x))) %>% dplyr::select(`Student`:`TestScore`))


}

################### hmk() ###################

#' Get homework score
#'
#' @description
#' Returns homework score(s) for any matches in student name.
#'
#' @param name string containing all or part of student name to search for
#'
#' @param data data frame of student info in tidy format
#'
#' @return a tibble with columns: Student, Homework, HmkScore, Section
#'
#' @details
#'
#' If a data frame of student info is not provided, the data stored in an internal environment with `set_student_data()` will be used instead.
#'
#' @export
#'
#' @examples
#' set_student_data("~/Downloads")
#' hmk("Sarah")

hmk <- function(name, data = NULL) {
  df <- find_student(name, data)
  if (nrow(df) > 0) {
    df %>%
    dplyr::distinct(Student, Homework, HmkScore, Section)
  } else {
    notfound()
  }
}

################# missed_test() #################

#' List students who missed tests
#'
#' @description
#' Returns students with NA for a particular test number
#'
#' @param num test number
#'
#' @param data database of student info
#'
#' @return a tibble with columns: Student, Section
#'
#' @export
#'
#' @examples
#' stud_df <- get_student_data("~/Downloads")
#' missed_test(1, stud_df)

missed_test <- function(num = 1, data = NULL) {
  if (is.null(data)) data <- studenv$studdata
  df <- data %>% dplyr::filter(Test == num) %>%
    dplyr::filter(is.na(TestScore))

  if (nrow(df) > 0) {
    df %>% dplyr::distinct(Student, Section)
  } else {
    notfound()
  }
}

################### lpr() ###################

#' Get number of late passes remaining
#'
#' @description
#' Returns number of late passes remaining (only applies to 1201)
#'
#' @param name string containing all or part of student name to search for
#'
#' @param data data frame of student info in tidy format
#'
#' @return a tibble with columns: Student, Late Passes Remaining, Section
#'
#' @export
#'
#' @examples
#' stud_df <- get_student_data("~/Downloads")
#' lpr("Jing", stud_df)


lpr <- function(name, data = NULL) {
  df <- find_student(name, data)
  if (nrow(df) > 0) {
    df %>%
    dplyr::distinct(Student, `Late Passes Remaining`, Section)
  } else {
    notfound()
  }
}

################### name2uni() ###################

#' What's the UNI?
#'
#' @description
#' Returns UNI(s) for any matches in student name
#' @inheritParams lpr
#'
#' @return a tibble with columns: Student, UNI, Section
#'
#' @export
#'
#' @examples
#' stud_df <- get_student_data("~/Downloads")
#' name2uni("Daisy", stud_df)

name2uni <- function(name, data = NULL) {

  df <- find_student(name, data)
  if (nrow(df) > 0) {
    df %>% dplyr::distinct(Student, UNI, Section)
  } else {
    notfound()
  }
}

################### tst() ###################

#' Get test score
#'
#' @description
#' Returns test score(s) for any matches in student name.
#'
#' @param name string containing all or part of student name to search for
#'
#' @param data data frame of student info in tidy format
#'
#' @return a tibble with columns: Student, Test, TestScore, Section
#'
#' @export
#'
#' @examples
#' stud_df <- get_student_data("~/Downloads")
#' tst("Emily")

tst <- function(name, data = NULL) {
  df <- find_student(name, data)
  if (nrow(df) > 0) {
    df %>%
    dplyr::distinct(Student, Test, TestScore, Section)
  } else {
  notfound()
    }
}

################### uni2name() ###################

#' What's the name?
#'
#' @description
#' Returns student name(s) for any match in student UNI
#'
#' @param uni string containing all or part of student UNI to search for
#'
#' @param data data frame of student info in tidy format
#'
#' @return a tibble with columns: Student, UNI, Section

#' @export
#'
#' @examples
#' stud_df <- get_student_data("~/Downloads")
#' uni2name("jtr13")

uni2name <- function(uni, data = NULL) {
  if (is.null(data)) data <- studenv$studdata
  df <- data %>% dplyr::filter(stringr::str_detect(UNI, uni))
  if (nrow(df) > 0) {
    df %>% dplyr::distinct(Student, UNI, Section)
  } else {
    notfound()
  }
}

################### wc() ###################

#' Which class?
#'
#' @description
#' Returns class name for any matches in student name
#' @inheritParams lpr
#'
#' @return a tibble with columns: Student, Section
#'
#' @export
#'

wc <- function(name, data = NULL) {

  df <- find_student(name, data)
  if (nrow(df) > 0) {
  df %>% dplyr::select(Student, Section) %>%
  dplyr::distinct(Student, Section)
  } else {
    notfound()
  }
}
