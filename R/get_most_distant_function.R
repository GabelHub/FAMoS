#' Get Most Distant Model
#'
#' This function can be used to find a model that is most distinct from all previously tested models.
#' @param input Either a string containing the directory which holds the AMS results folder or a matrix containing the tested models along with the respective information criteria. Default to \code{getwd()}.
#' @param mrun A string giving the number of the corresponding FAMoS run, e.g "004". If NULL (default), all FAMoS runs in the "AMS/ModelsTested/" folder will be used for evaluation.
#' @param max.number The maximum number of times that the \code{get.most.distant} function tries to find the most distant model (see details). Default to 100.
#' @details Taking the order from the 'ModelsTested' files found in 'AMS/ModelsTested/', this function successively tries to obtain a previously untested model that is most distant from all previously tested ones (here, distance means the number of difference in fitted parameters). To this end, a model is taken from 'ModelsTested' and its corresponding complement model is calculated (i.e. the model containing all parameters that the original didn't fit). From thereon, the distance of neighbouring models is calculated and the path of increasing distances is followed. This process is repeated until all models in 'ModelsTested' have been assessed or the \code{max.number} is reached.
#' @return A list containing in its first entry the maximal distance found, the second entry the parameter names and in its third entry the corresponding binary vector. Note that the model may not fulfill previously specified critical conditions.
#' @export
#'
#' @examples
#' get.most.distant(input = famos.run)
get.most.distant <- function(input = getwd(), mrun = NULL, max.number = 100){

  if(is.character(input)){
    #read in files (either a specific one or all)
    if(is.null(mrun)){
      filenames <- list.files(paste0(input,"/AMS/ModelsTested/"), pattern="*.rds", full.names=TRUE)
      if(length(filenames) == 0){
        stop("No files in the given folder!")
      }
      store.res <-  readRDS(filenames[1])
      for(i in 2:length(filenames)){
        store.res <- cbind(store.res, readRDS(filenames[i]))
      }
      mt <- store.res
    }else{
      if(file.exists(paste0(input,"/AMS/ModelsTested/ModelsTested",mrun,".rds")) == FALSE){
        stop("The specified file does not exist!")
      }
      mt <- readRDS(paste0(input,"/AMS/ModelsTested/ModelsTested",mrun,".rds"))
      if(is.null(mt)){
        stop("File is empty!")
      }
    }
  }else if(is.matrix(input)){
    mt <- input
  }else{
    stop("Input needs to be either a directory path or a matrix.")
  }

  #cut off header with IC and iteration number
  mt <- mt[5:nrow(mt),]
  for(k in 1:min(max.number, ncol(mt))){
    complement <- abs(mt[,k] - 1)
    distance.comp <- min(as.numeric(colSums(abs(mt-complement))))

    distance <- c()

    repeat{
      for(i in 1:length(complement)){
        comp.new <- complement
        comp.new[i] <- abs(complement[i] - 1)
        distance <- min(as.numeric(colSums(abs(mt-comp.new))))
        if(distance > distance.comp){
          distance.comp <- distance
          complement <- comp.new
          break
        }
      }
      if(i == length(complement)){

        if(distance <= distance.comp){
          break
        }

      }
    }
    if(k == 1 || (distance > best.distance)){
      best.distance <- distance
      best.comp <- complement
    }

  }

  return(list(c(distance = best.distance), names(which(best.comp == 1)), best.comp))
}