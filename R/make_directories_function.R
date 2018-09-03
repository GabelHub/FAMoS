#' Create Results Directories
#'
#' Creates the directories, in which the results are going to be saved.
#' @param homedir The directory in which the result folders will be created.
#' @details All files will be stored in the main folder "FAMoS-Results". It contains the following subdirectories:
#' \describe{
#'   \item{BestModel}{Contains the information criteria and the corresponding parameter estimates of the best fitting model.}
#'   \item{Figures}{Contains figures showing the performance of the FAMoS.}
#'   \item{Fits}{Contains the fitted parameter values of each of the tested models.}
#'   \item{LogFiles}{Contains the log files of the individual model fitting runs.}
#'   \item{TestedModels}{Contains the binary information of all of the tested models.}
#'   }
#'
#' @export
#' @return Creates directories.

make.directories <- function(homedir) {
  dir <- paste0(homedir, "/FAMoS-Results")
  if(!dir.exists(dir)){
    dir.create(dir, showWarnings = F)
  }
  if(!dir.exists(paste0(dir, "/BestModel"))){
    dir.create(paste0(dir, "/BestModel"), showWarnings = F)
  }
  if(!dir.exists(paste0(dir, "/Figures"))){
    dir.create(paste0(dir, "/Figures"), showWarnings = F)
  }
  if(!dir.exists(paste0(dir, "/Fits"))){
    dir.create(paste0(dir, "/Fits"), showWarnings = F)
  }
  if(!dir.exists(paste0(dir, "/LogFiles"))){
    dir.create(paste0(dir, "/LogFiles"), showWarnings = F)
  }
  if(!dir.exists(paste0(dir, "/TestedModels"))){
    dir.create(paste0(dir, "/TestedModels"), showWarnings = F)
  }
}
