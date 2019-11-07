############### Google Sheets functions ###############

#' Takes a downloaded zip of submissions from CourseWorks and uploads sheet of text entry information to Google Drive
#'
#' @description
#' Gets group number (or name) from file name, title from h1 tag, and content from p tags. Name of Google Sheet is determined by zip file name.
#'
#' Call only when creating a new sheet on GoogleDrive.
#'
#' @param path folder where student files are stored
#'
#' @param pattern pattern to match in filenames
#'
#' @export
#'
#' @examples
#' nss <- zip_to_drive("~/Downloads", "Final_Project_Groups.zip")
#' drive_browse(nss)

zip_to_drive <- function(path = "~/Downloads",
                       pattern = "test") {

  # get current files

  zipfile <- dir(path, pattern)

  if (length(zipfile) == 0) stop("Can't find a match.")
  if (length(zipfile) > 1) stop("Too many matches.")

  unzpath <- file.path(path, stringr::str_replace(zipfile, ".zip", "-unzipped"))

  unzip(file.path(path, zipfile), exdir = unzpath)
  allfiles <- list.files(unzpath)

  get_title <- function(node) node %>% rvest::html_node("h1") %>%
    rvest::html_text()
  # get_topic <- function(node) {
  #   p <- node %>% rvest::html_nodes("p") %>% rvest::html_text()
  #   span <- node %>% rvest::html_nodes("span") %>% rvest::html_text()
  #   paste(c(p, span), collapse = "")
  # }
  get_topic <- function(node) node %>% rvest::html_node("div") %>% rvest::html_text()

  group <- allfiles %>% stringr::str_remove("_.*") %>% readr::parse_number()
  df <- lapply(file.path(unzpath, allfiles), xml2::read_html)
  title <- lapply(df, get_title) %>% unlist()
  content <- lapply(df, get_topic) %>% unlist()
  df2 <- tibble::tibble(group, title, content) %>% dplyr::arrange(group)
  filename <- stringr::str_replace(zipfile, "zip", "csv")
  readr::write_csv(df2, file.path(path, filename))
  ss <- googledrive::drive_upload(file.path(path, filename),
                                  type = "spreadsheet")
  ss
}


