#' Plot FAMoS Performance
#'
#' For each FAMoS run \code{famos.performance} plots the corresponding best model and information criterion.
#' @param input Either a string giving the three-digit number of the corresponding FAMoS run, e.g "004", or a matrix containing the tested models along with the respective information criteria.
#' @param path If \code{input} is the string of an FAMoS run, the directory containing the AMS results folder needs to be supplied as well. Default to \code{\link{getwd()}}.
#' @param ic The information criterion the model selection will be based on. Options are "AICc", "AIC" and "BIC". Default to "AICc".
#' @param save.output A string containing the location and name under which the figure should be saved (format is .pdf). Default to NULL.
#' @details The upper plot shows the improvement of the selected information criterion over each FAMoS iteration. The best value is shown on the right axis. The lower plot depicts the corresponding best model of each iteration. Here, green circles show added, red circles removed and blue circles swapped parameters. The parameters of the final model are printed bold.
#' @return A plot showing the value of the corresponding information criterion and best model of each FAMoS iteration.
#' @export
#' @examples
#' #plot the performance of an FAMoS run
#' famos.performance(input = famos.run)


famos.performance <- function(input, path = getwd(), ic = "AICc", save.output = NULL){

  switch (ic,
          "AICc" = {ic.index <- 1},
          "AIC"  = {ic.index <- 2},
          "BIC"  = {ic.index <- 3}
  )
  if(is.character(input)){
  #read in the tested models
  if(file.exists(paste0(path,"/AMS/ModelsTested/ModelsTested",input,".rds")) == FALSE){
    stop("The specified file does not exist!")
  }
  mt <- readRDS(paste0(path,"/AMS/ModelsTested/ModelsTested",input,".rds"))
  if(is.null(mt) || ncol(mt) < 2){
    stop("famos.performance needs at least two completed runs for plotting.")
  }
  }else if(is.matrix(input)){
    mt <- input
  }else{
    stop("Input needs to be either a FAMoS run number or a matrix.")
  }
  #excluding failed runs
  if(length(which(is.na(mt[1,]))) > 0){
    mt <- mt[,-c(which(is.na(mt[1,])))]
  }
  all.names <- rownames(mt)[5:nrow(mt)]
  #create storage for individual runs and the corresponding number of jobs performed
  split.run <- list()
  jobs.per.run <- c()
  #split ModelsTested into smaller chunks per model run
  for(i in unique(mt[4,])){
    jobs.per.run <- c(jobs.per.run, length(which(mt[4,] == i)))
    split.run[[i]] <- mt[,which(mt[4,] == i)]
  }

  #get the corresponding best model of each run
  get.best <- mt[,1]
  for(i in unique(mt[4,-1])){
    runs <- as.matrix(split.run[[i]])
    if(ncol(runs) == 1){
      get.best <- cbind(get.best, runs)
    }else{
      get.best <- cbind(get.best, runs[,which.min(runs[ic.index,])[1]])
    }
  }

  #replace the model if no improvement in AICC is found
  for(i in 2:ncol(get.best)){
    if(get.best[ic.index,i] > get.best[ic.index,i-1]){
      get.best[,i] <- get.best[,i-1]
      get.best[4,i] <- get.best[4,i] + 1
    }
  }

  if(is.null(save.output) == FALSE){
    grDevices::pdf(file = save.output,
                   width  = ifelse(1.5 + 0.2*max(mt[4,]) < 4, 4 ,1.5 + 0.2*max(mt[4,])),
                   height = 2.5 + 0.2*(nrow(mt)-2),
                   useDingbats = F)
  }


    #plot improvement in AICc and the corresponding models
    graphics::layout(matrix(c(1,1,2,2),ncol = 2, byrow = TRUE),
                     widths=c(3,1), heights=c(1,3))

    graphics::par(mai = c(0,
                          0.5 + 0.06*max(nchar(all.names)),
                          0.2,
                          0.2 + 0.1 * nchar(as.character(round(get.best[ic.index,ncol(get.best)],1)))))
    #plot AICc
    graphics::plot(unique(mt[4,]), get.best[ic.index,],
                   log = "y",
                   xlab = "",
                   ylab = ic,
                   type = "o",
                   lwd = 2,
                   axes = F,
                   main = "FAMoS performance")
    graphics::box()
    graphics::axis(2, las = 1)
    graphics::axis(side   = 4,
                   at     = round(get.best[ic.index,ncol(get.best)],1),
                   tick   = TRUE,
                   las = 1)

    graphics::par(mai = c(1,
                          0.5 + 0.06 * max(nchar(all.names)),
                          0.2,
                          0.2 + 0.1 * nchar(as.character(round(get.best[ic.index,ncol(get.best)],1)))))
    #plot the corresponding models
    graphics::plot(0,0,
                   type = "n",
                   axes = F,
                   xlab = "run",
                   ylab = "",
                   xlim = c(1, max(mt[4,])),
                   ylim = c(1, nrow(mt) - 4))

    #plot the first model
    start <- which(mt[5:nrow(mt),1] == 1)
    graphics::points(rep(1, length(start)), length(all.names) + 1 - start , pch = 4)

    #plot the following best models and indicate the changes by coloured circles
    for(i in unique(mt[4,-1])){
      rowx <- which(get.best[4,] == i)
      start <- which(get.best[5:nrow(get.best),rowx] == 1)
      graphics::points(rep(i, length(start)), length(all.names) + 1 - start , pch = 4)
      diff.model <- (get.best[5:nrow(get.best),rowx] - get.best[5:nrow(get.best),rowx-1])
      if(sum(abs(diff.model)) > 0){
        if(sum(diff.model) == 0){
          crcl.col <- "blue"
        }else if(max(diff.model) == 1){
          crcl.col <- "chartreuse4"
        }else{
          crcl.col <- "red"
        }
        crcl <- which(diff.model != 0)
        graphics::points(rep(i, length(crcl)), length(all.names) + 1 - crcl , pch = 1, col = crcl.col, cex = 2, lwd = 2)
      }
    }

    graphics::box()
    graphics::axis(1, las = 1)

    for(i in 1:length(all.names)){
      graphics::axis(side   = 2,
                     at     = length(all.names) + 1 - i,
                     labels = all.names[i],
                     tick   = TRUE,
                     las = 2,
                     cex.axis = 0.7,
                     font = get.best[4 + i,ncol(get.best)] + 1)
    }


  if(is.null(save.output) == FALSE){
    grDevices::dev.off()

  }

}
