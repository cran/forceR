#' Sorts files after corrections
#'
#' The files of each of the various possible correction steps (cropping, amplififer correction, drift correction)
#' are all located in their own folders. This function gets all files that represent the last correction step of a
#' given measurement out of all those folders and saves them in the `results.folder`.
#'
#' @param data.folders Character vector containing full folder paths of folders to check. This list must be sorted
#'   according to the chronology of previous file editing. If a measurement exists in the last folder,
#'   this is copied or moved into the `results.folder`, and files of the same measurement located in the
#'   other folders will be ignored. Hence, the one file per measurement that underwent most correction steps
#'   will be stored in the `results.folder`, while the rest of the files of the same measurement remain in place.
#' @param results.folder Character string defining the full path to the folder where the desired files will be stored.
#' @param move A logical value specifying if files should be moved (`move = TRUE`) or copied (`move = FALSE`). Default: `FALSE`.
#' @return This functions does not create new files but sorts existing files. It does, however, create the `results.folder`
#' in case it did not exist before.
#' @details The function will look for leading numbers in the file names specifying the measurement number to find corresponding
#' files in the different folders. E.g., it will identify "0001_ABCD.csv", "0001_ABCD_ampdriftcorr.csv", and
#' "0001_ABCD_ampdriftcorr_baselincorr.csv" as stemming from the same measurement and sort them accordingly.
#' @examples
#'# define data.folders
#'data.folders <- c("./raw",
#'                    "./cropped",
#'                    "./ampdriftcorr",
#'                    "./baselinecorr")
#'
#'# define the folder in which one corrected file per original raw measurement
#'#   should be stored.
#'results.folder <- "./corrected"
#'
#'# run the file sorting - commented out to pass package tests
#'# sort_files(data.folders = data.folders,
#'#               results.folder = results.folder,
#'#               move = FALSE)
#' @export
sort_files <- function(data.folders,
                       results.folder,
                       move = FALSE){

  if(!is.character(data.folders)) stop ("'data.folders' must contain character strings only.")
  if(length(data.folders) <= 1) stop ("'data.folders' only contains one string - sorting is not necessary.")

  if(!is.character(results.folder)) stop ("'results.folder' must contain character string only.")
  if(length(results.folder) > 1) stop ("'results.folder' must only contain one character string.")

  if(!is.logical(move)) stop ("'move' must be logical.")

  if(!dir.exists(results.folder)){
    dir.create(results.folder, showWarnings = FALSE)
  } else {
    # print(results.folder, " already exists!")
  }

  for (i in length(data.folders):2) {
    # 1 <- length(data.folders)
    filelist.last <- list.files(data.folders[i], pattern = "\\.csv")
    filelist.results.folder <- list.files(results.folder, pattern = "\\.csv")

    # find files that are not yet present in results folder
    measurements.last <- gsub("(^\\d+).+", "\\1", filelist.last)
    measurements.results <- gsub("(^\\d+).+", "\\1", filelist.results.folder)
    measurements.new <- setdiff(measurements.last, measurements.results)
    files.only.in.last <- filelist.last[which(measurements.last %in% measurements.new)]
    for(file in files.only.in.last){
      # move/copy all files that were only ampdriftcorred but not baselinecorred into corrected directory
      if(move == TRUE){
        # print(paste0("moving ", file, "..."))
          file.move(file.path(data.folders[i], file),
                  results.folder,
                  overwrite = TRUE)
      } else{
        # print(paste0("copying ", file, "..."))
        file.copy(file.path(data.folders[i], file),
                  results.folder)
      }
    }
  }


}
