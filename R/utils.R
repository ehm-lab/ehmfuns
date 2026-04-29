#' Construct Paths Relative to user-environment-specific V-Drive Root
#'
#' @description
#' Prefixes paths with the "V_DRIVE" environment variable. \cr
#' Set it by running \code{usethis::edit_r_environ()} in the console, opening
#' .Renviron, and adding V_DRIVE = yourVDRIVEpath.\cr
#' Facilitates script collaboration by removing hard-coded VDRIVE which may
#' appear differently to different users.\cr
#'
#' @param ... Character vector passed to \code{file.path()}.
#' @return A character vector of the full file path.
#' @export
vd_fp <- function(...) {
  root <- Sys.getenv("V_DRIVE")
  if (root == "") stop("V_DRIVE root path not found! Set it by running
                       *usethis::edit_r_environ()* in the console, opening
                       .Renviron, and adding V_DRIVE = yourVDRIVEpath")
  file.path(root, ...)
}

#' Mirror Files from Network Drive to Local Project
#'
#' @description
#' Efficiently copies files from a source root to a local directory using
#' `robocopy` (Windows) or `rsync` (macOS/Linux). \cr
#' Assumes destination folder name and V_DRIVE env. var.
#'
#' @param relative_paths Character vector of paths relative to `source_root`.
#' @param source_root Base directory (defaults to `V_DRIVE` env var).
#' @param local_root Destination directory (defaults to `./data`).
#' @export
copy_to_local <- function(relative_paths,
                          source_root = Sys.getenv("V_DRIVE"),
                          local_root = "./data") {

  if (source_root == "") stop("Source root not found!")
  is_win <- .Platform$OS.type == "windows"

  for (path in relative_paths) {
    src  <- file.path(source_root, path)
    dest <- file.path(local_root, path)

    if (!dir.exists(dirname(dest))) dir.create(dirname(dest), recursive = TRUE)

    if (is_win) {
      system2("robocopy", args = c(shQuote(dirname(src)), shQuote(dirname(dest)),
                                   shQuote(basename(src)), "/z", "/njh", "/njs"))
    } else {
      system2("rsync", args = c("-auP", shQuote(src), shQuote(dest)))
    }
  }
  invisible(NULL)
}
