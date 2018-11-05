#
library(tidyverse)

section_names <- tibble(oldname = c("STATUN1201_001_2018_3 - CALC-BASED INTRO TO STATISTICS", "STATUN1201_002_2018_3 - CALC-BASED INTRO TO STATISTICS", "STATGR5702_001_2018_3 - EXPLORATORY DATA ANALYSIS/VISUAL"), newname = c("MW", "TT", "EDAV"))

path <- "~/Documents/Students"

fixfiles <- function(filename, path) {
  df <- read_csv(file.path(path,filename)) %>%
    filter(!str_detect(Student, "Points")) %>%
    filter(Student != "Student, Test")

  # get rid of numbers in parentheses
  colnames(df) <-
  str_remove_all(colnames(df)," \\([0-9]+\\)")

  # choose columns
  df <- df %>%
    select(`Student`:`Homework Current Points`)

  # change section name
  sect <- df$Section[1]
  df$Section <- section_names$newname[section_names$oldname == sect]

  # tidy homework and test columns
  df <- df %>%
    gather(key = "Homework", value = "HmkScore",
           starts_with("Homework")) %>%
    mutate(Homework = parse_number(Homework)) %>%
    gather(key = "Test", value = "TestScore",
           starts_with("Test")) %>%
    mutate(Test = parse_number(Test))

  write_csv(df, file.path(path, paste0("NEW", filename)))


}


# get rid of previous NEW files

newfiles <- file.path(path, dir(path, pattern = "NEW"))

if (length(newfiles) > 0) map(newfiles, file.remove)


# get current files

files <- dir(path, pattern = "Fall_2018")

# create NEW files
map(files, fixfiles, path = path)

# read and combine NEW files

files2 <- dir("~/Documents/Students", pattern = "NEW")

students <- map_df(files2, ~read_csv(file.path(path, .x))) %>% select(`Student`:`TestScore`)

get_student <- function(name) {
  df <- students %>%
    filter(grepl(name, Student))
  if (nrow(df) < 1) df <- NULL
  return(df)
}

wc <- function(name) {
  df <- get_student(name)
  if (!is.null(df))
    df %>% select(Student, Section) %>%
    distinct(Student, Section)
}

hmk <- function(name, num) {
  df <- get_student(name)
  if (!is.null(df))
    df %>%
      filter(Homework == num) %>%
      distinct(Student, Homework, HmkScore, Section)
}
tst <- function(name, num) {
  df <- get_student(name)
  if (!is.null(df))
    df %>%
    filter(Test == num) %>%
    distinct(Student, Test, TestScore, Section)
}

lpr <- function(name) {
  df <- get_student(name)
  if (!is.null(df))
    df %>%
    distinct(Student, `Late Passes Remaining`, Section)
}

missed_test <- function(num = 1) {
  students %>% filter(Test == num) %>%
    filter(is.na(TestScore)) %>%
    distinct(Student, Section)
}

