#' @importFrom magrittr "%>%"

fixfiles <- function(filename, path) {
  df <- readr::read_csv(file.path(path,filename)) %>%
    dplyr::filter(!stringr::str_detect(Student, "Points")) %>%
    dplyr::filter(Student != "Student, Test")

  # get rid of numbers in parentheses
  colnames(df) <-
  stringr::str_remove_all(colnames(df)," \\([0-9]+\\)")

  # choose columns
  df <- df %>%
    dplyr::select(`Student`:`Homework Current Points`)

  # change section name
  sect <- df$Section[1]
  df$Section <- section_names$newname[section_names$oldname == sect]

  # tidy homework and test columns
  df <- df %>%
    tidyr::gather(key = "Homework", value = "HmkScore", dplyr::starts_with("Homework")) %>%
    dplyr::mutate(Homework = readr::parse_number(Homework)) %>%
    tidyr::gather(key = "Test", value = "TestScore",
           dplyr::starts_with("Test")) %>%
    dplyr::mutate(Test = readr::parse_number(Test))

  readr::write_csv(df, file.path(path, paste0("NEW", filename)))

}


get_student <- function(name, students) {
  df <- students %>%
    dplyr::filter(grepl(name, Student))
  if (nrow(df) < 1) df <- NULL
  return(df)
}

