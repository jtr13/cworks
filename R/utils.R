# suppress readr info
options(readr.num_columns = 0)

fixfiles <- function(filename, path) {
  df <- readr::read_csv(file.path(path,filename)) %>%
    dplyr::filter(!stringr::str_detect(Student, "Points")) %>%
    dplyr::filter(Student != "Student, Test")

  # get rid of numbers in parentheses
  colnames(df) <-
  stringr::str_remove_all(colnames(df)," \\([0-9]+\\)")

  df <- df %>% dplyr::rename(UNI = `SIS User ID`) %>%
    dplyr::select(-ID, -`SIS Login ID`)

  # choose columns
  # (the select looks odd since the column before Homework Current Points varies)
  df <- df %>%
    dplyr::select(`Student`:`Homework Current Points`) %>% select(-`Homework Current Points`)

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


find_student <- function(name, data) {
  if (is.null(data))
    data <- studenv$studdata
  data %>%
    dplyr::filter(stringr::str_detect(Student, name))
}


notfound <- function() {
  message("No matches found.")
}
