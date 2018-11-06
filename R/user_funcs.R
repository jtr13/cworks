#' @export

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

#' @export

create_db <- function(path = "~/Documents/Students", pattern = "NEW") {

  files <- dir(path, pattern)

  purrr::map_df(files, ~readr::read_csv(file.path(path, .x))) %>% dplyr::select(`Student`:`TestScore`)
}


#' @export

wc <- function(name, students) {
  df <- get_student(name, students)
  if (!is.null(df))
    df %>% dplyr::select(Student, Section) %>%
    dplyr::distinct(Student, Section)
}

#' @export
#'
hmk <- function(name, num, students) {
  df <- get_student(name, students)
  if (!is.null(df))
    df %>%
    dplyr::filter(Homework == num) %>%
    dplyr::distinct(Student, Homework, HmkScore, Section)
}

#' @export
tst <- function(name, num, students) {
  df <- get_student(name, students)
  if (!is.null(df))
    df %>%
    dplyr::filter(Test == num) %>%
    dplyr::distinct(Student, Test, TestScore, Section)
}

#' @export
lpr <- function(name, students) {
  df <- get_student(name, students)
  if (!is.null(df))
    df %>%
    dplyr::distinct(Student, `Late Passes Remaining`, Section)
}

#' @export

missed_test <- function(num = 1, students) {
  students %>% dplyr::filter(Test == num) %>%
    dplyr::filter(is.na(TestScore)) %>%
    dplyr::distinct(Student, Section)
}

