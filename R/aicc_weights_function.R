#' Plot evidence ratios
#'
#' Calculates the evidence ratios for each parameter based on Akaike weights and plots them.
#' @param input Either a string containing the directory which holds the AMS results folder or a matrix containing the tested models along with the respective information criteria. Default to \code{getwd()}.
#' @param mrun A string giving the number of the corresponding FAMoS run, e.g "004". If NULL (default), all FAMoS runs in the "AMS/ModelsTested/" folder will be used for evaluation.
#' @param reorder If TRUE, results will be ordered by evidence ratios (descending). If FALSE, the order of parameters will be the same as the order specified in \code{init.par} in \code{\link{famos}}. Default to TRUE.
#' @param save.output A string containing the location and name under which the figure should be saved (format is pdf). Default to NULL.
#' @details The plot shows the relative support or evidence ratio for each parameter. Parameters included in the best model are printed bold and the corresponding lines are coloured in red.
#' @return A plot showing the evidence ratios for all model parameters. Additionally, the evidence ratios are returned.
#' @export
#' @examples
#' #plot evidence ratios
#' aicc.weights(input = famos.run)
#' aicc.weights(input = famos.run, reorder = FALSE)
aicc.weights <- function(input = getwd(), mrun = NULL, reorder = TRUE, save.output = NULL){
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

  if(length(which(is.finite(mt[1,]) == FALSE)) > 0){
    mt <- mt[,-which(is.finite(mt[1,]) == FALSE)]
  }

  #calculate akaike weights
  akaike.weights <- as.numeric(exp(-0.5 * (mt[1,] - min(mt[1,])))/sum(exp(-0.5 * (mt[1,] - min(mt[1,])))))
  #calculate normalised probability
  parms.support <- as.vector(as.matrix(mt[5:nrow(mt),]) %*% akaike.weights)
  names(parms.support) <- row.names(mt[5:nrow(mt),])
  if(reorder == TRUE){
    parms.order <- order(parms.support, decreasing = FALSE)
  }else{
    parms.order <- length(parms.support):1
  }

  #adjust color scheme
  best.model <- mt[-c(1:4),which.min(mt[1,])]
  aicc.col <- rep("blue", length(best.model))
  aicc.col[which(best.model == 1)] <- "red"
  #adjust boldness of parameters
  aicc.bold <- rep(1, length(best.model))
  aicc.bold[which(best.model == 1)] <- 2
  #get parameter names
  all.names <- rownames(mt)[5:nrow(mt)]

  #save file if wanted
  if(is.null(save.output) == FALSE){

    grDevices::pdf(file = save.output,
                   width  = 5,
                   height = 1.5 + 0.2*length(parms.support),
                   useDingbats = F)

  }

    graphics::par(mfrow = c(1,1),mai = c(1,0.2 + 0.08*max(nchar(all.names)),0.2,0.2))
    graphics::plot(0,0,
                   xlim = c(0,1),
                   ylim = c(1,length(parms.support)),
                   type = "n",
                   axes = FALSE,
                   xlab = "relative support",
                   ylab = "")
    for(i in 1:length(parms.support)){
      graphics::lines(c(0,parms.support[parms.order[i]]),c(i,i),
                      lwd = 2,
                      col = aicc.col[parms.order[i]])
    }

    graphics::box()
    graphics::axis(1, las = 1)

    for(i in 1:length(all.names)){
      graphics::axis(side   = 2,
                     at     = i,
                     labels = all.names[parms.order][i],
                     tick   = TRUE,
                     las = 2,
                     cex.axis = 0.7,
                     font = aicc.bold[parms.order[i]])
    }

  if(is.null(save.output) == FALSE){
    grDevices::dev.off()
  }

  return(parms.support)
}