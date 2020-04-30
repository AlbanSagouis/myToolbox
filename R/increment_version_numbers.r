#' Increment version number of a package
#'
#' Reads DESCRIPTION from installed package 'pkg' and makes a new one in the specified
#''folder'. Three options for 'increase' are "minor", "patch" and "dev" respectively
#' incrementing the second, third and fourth elements of version 0.1.0.9012.
#'
#' Also updates the date to system date.
#'
#' @param pkg a character vector containing complete
#' @param folder description
#' @param increase description
#,
#' @return NULL
#' @details description. Originally posted by alko989 here https://stackoverflow.com/q/24209336. Hadley Wickham discusses when and how increment version numbers in his book http://r-pkgs.had.co.nz/release.html#release-version
#'
#' @author alko989, Alban Sagouis
#'
#'
#' @export


incVer <- function(pkg, folder=".", increase="dev"){

   f <- read.dcf(file=system.file("DESCRIPTION", package=pkg),
                 fields=c("Package", "Type", "Title", "Version", "Date",
                          "Author", "Maintainer", "Description", "License",
                          "Depends", "Imports", "Suggests",
                          "Encoding", "LazyData", "RoxygenNote"))
   curVer <- package_version(f[4])
   if(increase == "patch") {
      curVer[[1,3]] <- ifelse(is.na(curVer$patchlevel), 1, curVer$patchlevel + 1)

   } else if (increase == "minor") {
      curVer[[1,2]] <- ifelse(is.na(curVer$minor), 1, curVer$minor + 1)
      curVer[[1,3]] <- 0
      curVer[[1,4]] <- 0
   } else if (increase == "dev") {
      curVer[[1,4]] <- ifelse(is.na(curVer[[1,4]]), 9001, curVer[[1,4]] + 1)

   } else {
      stop(paste("Can not identify the increase argument: " , increase))
   }

   f[4] <- toString(curVer)
   ## Update also the date
   f[5] <- format (Sys.time(), "%Y-%m-%d")
   write.dcf(f, file=paste(folder, "DESCRIPTION", sep="/"))

   if(increase == 'minor' | increase == 'patch') print(
      "Consider updating README.md and NEWS.md."
   )

}
