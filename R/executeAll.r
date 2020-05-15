#' Execute one script in the background not loading anything in the user's current environment
#'
#' Source the given file with a .r or .R extension.
#'
#' @param fullPath A character vector with the path to the scripts to execute
#,
#' @return NULL
#' @details If not working inside an Rproject, path has to be complete from root.
#'
#' @author Alban Sagouis
#'
#'
#' @export

executeOne <- function(fullPath = NULL) {
   source(fullPath, encoding = 'UTF-8', echo = FALSE, local = TRUE)
}

#' Execute all scripts inside a folder
#'
#' Source all files with a .r or .R extension in the specified folder.
#'
#' @param fullPath A character vector with the path to the folder containing the scripts to
#' execute
#,
#' @return NULL
#' @details If not working inside an Rproject, path has to be complete from root.
#'
#' @author Alban Sagouis
#'
#'
#' @export

executeAll <- function(fullPath = NULL) {
   # if(substr(path, 1, 1) %in% c('~', '.')) path <- path.expand(path)
   listF <- list.files(path, pattern = ".R|.r", full.names = TRUE)
   lapply(listF, executeOne)
}
