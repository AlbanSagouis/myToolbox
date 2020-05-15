#' Replace text in one script
#'
#' Open a file with a .r or .R extension and runs a gsub.
#'
#' @param fullPath A character vector with the path to the script to modify.
#' @param pattern passed to /code{gsub}.
#' @param replacement passed to /code{gsub}.
#' @param newPath description
#,
#' @return NULL
#' @details If not working inside an Rproject, path has to be complete from root.
#' @examples
#' fullPath <- "C:/Users/as80fywe/idiv/my r packages/myToolbox/tests/testthat/objects/random_script.r"
#' fullPath <- "./tests/testthat/objects/random_script.r"
#' gsubInOneScript(fullPath, pattern="random_2020", replacement = "newID_2021")
#'
#' @author Alban Sagouis
#' @export

gsubInOneScript <- function(fullPath, pattern, replacement, newPath = NULL) {
   sourceF <- file(fullPath, open = "r+b")
   lines <- readLines(sourceF)

   for(i in seq_along(lines)) {
      lines[i] <- gsub(lines[i], pattern=pattern, replacement=replacement)
   }

   if(!is.null(newName)) sourceF <- file(newName, open = "w")

   writeLines(text = lines, con = sourceF, sep = "\n")
   close(sourceF)  # closing connection
}

# warning before for backup?


#' Replace text in all scripts from a folder
#'
#' Open a file with a .r or .R extension and runs a gsub.
#'
#' @param fullPath A character vector with the path to the folder countaining
#' the scripts to modify.
#' @param pattern passed to /code{gsub}.
#' @param replacement passed to /code{gsub}.
#,
#' @return NULL
#' @details If not working inside an Rproject, path has to be complete from root.
#' @examples
#' fullPath <- "C:/Users/as80fywe/idiv/my r packages/myToolbox/tests/testthat/objects/"
#' fullPath <- "./tests/testthat/objects/"
#' gsubInOneScript(fullPath, pattern="random_2020", replacement = "newID_2021")
#'
#' @author Alban Sagouis
#' @export

gsubInOneFolder <- function(fullPath, pattern, replacement) {
   listF <- list.files(fullPath, pattern = ".R|.r", full.names = TRUE)
   lapply(listF, gsubInOneScript, pattern, replacement)
}

# warning before for backup?


