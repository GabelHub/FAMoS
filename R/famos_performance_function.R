#' Plot FAMoS Performance
#'
#' For each FAMoS run \code{famos.performance} plots the corresponding best model and selection criterion value.
#' @param input Either a string giving the three-digit number of the corresponding FAMoS run, e.g "004", or a matrix containing the tested models along with the respective information criteria.
#' @param path If \code{input} is the string of an FAMoS run, the directory containing the "FAMoS-Results" folder needs to be supplied as well. Default to \code{\link{getwd}}.
#' @param reattempts A numeric vector containing other SCVs that should be plotted. Used mainly for reattempts in the main famos routine.
#' @param save.output A string containing the location and name under which the figure should be saved (format is .pdf). Default to NULL.
#' @param ... Other graphical parameters.
#' @details The upper plot shows the improvement of the selection criterion over each FAMoS iteration. The best value is shown on the right axis. The lower plot depicts the corresponding best model of each iteration. Here, green colour shows added, red colour removed and blue colour swapped parameters. The parameters of the final model are printed bold.
#' @return A plot showing the value of the selection criterion and best model of each FAMoS iteration.
#' @export
#' @examples
#' #plot the performance of an FAMoS run
#' famos.performance(input = famos.run)


famos.performance <- function(input, path = getwd(), reattempts = NULL, save.output = NULL, ...){
  
  old.mai <- graphics::par("mai")
  old.mfrow <- graphics::par("mfrow")
  on.exit(graphics::par(mai = old.mai, mfrow = old.mfrow))
  
  
  if(is.character(input)){
    #read in the tested models
    if(file.exists(paste0(path,"/FAMoS-Results/TestedModels/TestedModels",input,".rds")) == FALSE){
      stop(paste0("The specified file in ", paste0(path,"/FAMoS-Results/TestedModels/TestedModels",input,".rds"), " does not exist. If the directory is incorrect, use 'path' to specify a new one."))
    }
    mt <- readRDS(paste0(path,"/FAMoS-Results/TestedModels/TestedModels",input,".rds"))
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
  all.names <- rownames(mt)[3:nrow(mt)]
  #create storage for individual runs and the corresponding number of jobs performed
  split.run <- list()
  jobs.per.run <- c()
  #split TestedModels into smaller chunks per model run
  for(i in unique(mt[2,])){
    jobs.per.run <- c(jobs.per.run, length(which(mt[2,] == i)))
    split.run[[i]] <- mt[,which(mt[2,] == i)]
  }
  
  #get the corresponding best model of each run
  get.best <- mt[,1]
  for(i in unique(mt[2,-1])){
    runs <- as.matrix(split.run[[i]])
    if(ncol(runs) == 1){
      get.best <- cbind(get.best, runs)
    }else{
      get.best <- cbind(get.best, runs[,which.min(runs[1,])])
    }
  }
  
  #replace the model if no improvement in SCV is found
  for(i in 2:ncol(get.best)){
    if(get.best[1,i] >= get.best[1,i-1]){
      get.best[,i] <- get.best[,i-1]
      get.best[2,i] <- get.best[2,i] + 1
    }
  }
  
  if(is.null(save.output) == FALSE){
    grDevices::pdf(file = save.output,
                   width  = ifelse(1.5 + 0.2*max(mt[2,]) < 4, 4 ,1.5 + 0.2*max(mt[2,])),
                   height = 2.5 + 0.2*(nrow(mt)-2),
                   useDingbats = F)
  }
  
  
  #plot improvement in SCV and the corresponding models
  graphics::layout(matrix(c(1,1,2,2),ncol = 2, byrow = TRUE),
                   widths=c(3,1), heights=c(1,3))
  
  graphics::par(mai = c(0,
                        0.5 + 0.06*max(nchar(all.names)),
                        0.2,
                        0.2 + 0.1 * nchar(as.character(round(get.best[1,ncol(get.best)],1)))))
  #plot SCV
  #log graphic scale if appropriate
  if(min(get.best[1,]) > 0 && (min(get.best[1,])/max(get.best[1,])) < 3*10^-2 && 
     !is.element("log", names(list(...)))){
    graphics.list <- c(list(...), list(log = "y"))
  }else{
    graphics.list <- list(...)
  }
  
  do.call(graphics::plot, c(list(x = unique(mt[2,]),
                                 y = get.best[1,],
                                 xlab = "",
                                 ylab = "SC value",
                                 type = "o",
                                 lwd = 2,
                                 axes = F,
                                 main = "FAMoS performance"),
                            graphics.list))
  
  #plot additional attempts
  if(!is.null(reattempts)){
    graphics::lines(x = unique(mt[2,]), y = reattempts, type = "o", lty = 2, col = "gray")
  }
  graphics::box()
  graphics::axis(2, las = 1, cex.axis = 0.7)
  graphics::axis(side   = 4,
                 at     = round(get.best[1,ncol(get.best)],1),
                 tick   = TRUE,
                 las = 1,
                 cex.axis = 0.7)
  
  graphics::par(mai = c(1,
                        0.5 + 0.06 * max(nchar(all.names)),
                        0.2,
                        0.2 + 0.1 * nchar(as.character(round(get.best[1,ncol(get.best)],1)))),
                xpd = FALSE)
  
  
  image.matrix <- get.best[-c(1:2),]
  plot.matrix <- image.matrix
  for(i in 2:ncol(image.matrix)){
    image.matrix[,i] <- get.best[-c(1:2),i] - get.best[-c(1:2),i-1]
    if(is.element(-1, image.matrix[,i]) && is.element(1, image.matrix[,i])){
      plot.matrix[which(image.matrix[,i] == 1),i] <- 4
      plot.matrix[which(image.matrix[,i] == -1),i] <- 4
    }else if(is.element(-1, image.matrix[,i])){
      plot.matrix[which(image.matrix[,i] == -1),i] <- 2
    }else if(is.element(1, image.matrix[,i])){
      plot.matrix[which(image.matrix[,i] == 1),i] <- 3
    }
  }
  
  for(i in 1:ncol(plot.matrix)){
    plot.matrix[,i] <- rev(plot.matrix[,i])
  }
  
  graphics::image( 1:ncol(get.best),1:(nrow(get.best) - 2), t(plot.matrix),
                   col = c("white", "gray40","red", "chartreuse4" , "blue"),
                   breaks = c(0,0.5,1.5,2.5,3.5,4.5),
                   xlab = c("iteration"),
                   ylab = c(""),
                   axes = F)
  
  graphics::box()
  graphics::axis(1, las = 1)
  
  for(i in 1:length(all.names)){
    graphics::axis(side   = 2,
                   at     = length(all.names) + 1 - i,
                   labels = all.names[i],
                   tick   = TRUE,
                   las = 2,
                   cex.axis = 0.7,
                   font = get.best[2 + i,ncol(get.best)] + 1)
  }
  
  graphics::grid(nx = ncol(plot.matrix), ny = nrow(plot.matrix), lty = 1)
  
  if(is.null(save.output) == FALSE){
    grDevices::dev.off()
    
  }
  
}
