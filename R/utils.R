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
  if (root == "") stop(
    "V_DRIVE root path not found! Set it by running *usethis::edit_r_environ()* \n
    in the console, opening .Renviron, and adding V_DRIVE = yourVDRIVEpath"
    )
  file.path(root, ...)
}

#' Efficiently copies specific files or directories to a target directory using OS-native tools
#'
#' @param relative_paths Character vector of file or folder paths relative to `source_root`.
#' @param destination_folder Exact folder path where files will be copied to.
#' @param source_root Base/project directory.
#' @return Invisible NULL.
#' @export
copy_utility <- function(
    relative_paths,
  destination_folder = "",
  source_root = "") {

  # Verify source root is provided
  if (source_root == "") stop("Source root not found!")

  # Ensure destination directory exists
  if (!dir.exists(destination_folder)) {
    dir.create(destination_folder, recursive = TRUE)
  }

  # Detect operating system to select robocopy (Windows) or rsync (Unix)
  is_win <- .Platform$OS.type == "windows"

  for (path in relative_paths) {
    src <- file.path(source_root, path)
    is_dir <- isTRUE(file.info(src)$isdir)

    if (is_win) {
      if (is_dir) {
        dest_path <- file.path(destination_folder, basename(src))
        system2("robocopy", args = c(shQuote(src), shQuote(dest_path), "/E", "/z", "/njh", "/njs"))
      } else {
        system2("robocopy", args = c(shQuote(dirname(src)), shQuote(destination_folder), shQuote(basename(src)), "/z", "/njh", "/njs"))
      }
    } else {
      system2("rsync", args = c("-auP", shQuote(src), shQuote(destination_folder)))
    }
  }

  invisible(NULL)
}

