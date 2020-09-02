#' ## https://github.com/matt-jay/rjaytools/blob/master/R/get_file_path.R  License: GPL-3
#'
#'
#' #' Create a character string for an absolute, fully resolved file path.
#' #'
#' #' This function constructs a full file path string for use in reading from or writing to the file system. It is always using a base path by default, and adds any additional relative path under it when one is passed to the function.
#' #'
#' #' The base path defaults to the user directory (\code{~}), and can be defined with an environment variable \code{JT_LOCAL_PATH}.
#' #'
#' #' @param relativePath Optional. A character string defining the relative path under the base path. See the Details section for more on the base path.
#' #'
#' #' @return A character string with the full expanded path (using \link{path.expand}.
#' #'
#' #' @examples
#' #' get_file_path('my/storage/path')
#' #' get_file_path('my/storage/path/for/this_file.csv')
#' #'
#' #' Sys.setenv(JT_LOCAL_PATH = '~/my/custom/storage/path')
#' #' get_file_path('for/this_file.csv')
#'
#' get_file_path <- function(relativePath = "") {
#'
#'    basePath <- ifelse(Sys.getenv("JT_LOCAL_PATH") == "", "~", Sys.getenv("JT_LOCAL_PATH"))
#'
#'    fullPath <- paste(
#'       path.expand(basePath),
#'       "/",
#'       relativePath,
#'       sep = "",
#'       collapse = ""
#'    )
#'
#'    return(fullPath)
#' }
