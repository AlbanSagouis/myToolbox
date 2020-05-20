#' Replace text in one script
#'
#' Open a file with a .r or .R extension and runs a gsub.
#'
#' @param fullPath A character vector with the path to the script to modify.
#' @param pattern passed to \code{\link[base:grep]{base::gsub}}.
#' @param replacement passed to \code{\link[base:grep]{base::gsub}}.
#' @param newPath Character vector. Used if a copy of the script should be created
#' following this path.
#'
#' @return SHOULD RETURN THE NUMBER OF MATCHES
#'
#' @details If not working inside an Rproject, path has to be complete from root.
#' @seealso To search for text in any text file, see \code{search_files}.
#' @author Alban Sagouis
#' @export

gsubInOneScript <- function(fullPath, pattern, replacement, newPath = NULL) {
   sourceF <- file(fullPath, open = "r+b")
   lines <- readLines(sourceF)

   for(i in seq_along(lines)) {
      lines[i] <- gsub(lines[i], pattern=pattern, replacement=replacement)
   }

   if(!is.null(newPath)) sourceF <- file(newPath, open = "w")

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
#' @param pattern passed to \code{\link[base:grep]{base::gsub}}.
#' @param replacement passed to \code{\link[base:grep]{base::gsub}}.
#' @param recursive logical. Passed to \code{\link[base:list.files]{base::list.files}}. Should the listing recurse into directories?
#' @param newPath description
#'
#' @return SHOULD RETURN THE NUMBER OF MATCHES PER SCRIPTS
#'
#' @details If not working inside an Rproject, path has to be complete from root.
#' @author Alban Sagouis
#' @export

gsubInOneFolder <- function(fullPath, pattern, replacement, recursive = FALSE, newPath = NULL) {
   listF <- list.files(fullPath, pattern = ".R|.r", full.names = TRUE, recursive = recursive)
   lapply(listF, gsubInOneScript, pattern, replacement, newPath)
}

# warning before for backup?


