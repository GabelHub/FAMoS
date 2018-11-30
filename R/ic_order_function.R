#' Plot Model Information Criteria
#'
#' Plots the information criteria of the tested models in ascending order.
#' @param input Either a string containing the directory which holds the "FAMoS-Results" folder or a matrix containing the tested models along with the respective information criteria. Default to \code{getwd()}.
#' @param mrun A string giving the number of the corresponding FAMoS run, e.g "004". If NULL (default), all FAMoS runs in the folder will be used for evaluation.
#' @param number Specifies the number of models that will be plotted. If NULL (default), all tested models will be used for plotting.
#' @param ic  The information criterion the model selection will be based on. Options are "AICc", "AIC" and "BIC". Default to "AICc".
#' @param colour.par The name of a model parameter. All models containing this parameter will be coloured red. Default to NULL.
#' @param save.output A string containing the location and name under which the figure should be saved (format is .pdf). Default to NULL.
#' @param ... Additional parameters that will be passed on to \code{\link{barplot}}.
#' @return Barplot showing the ordered information criteria of the tested models. Also returns a data frame containing each unique tested model with its best information criteria.
#' @export
#' @examples
#' #plot the information criteria
#' ic.order(input = famos.run, log = "y")
#' ic.order(input = famos.run, log = "y", colour.par = "p1")

ic.order <- function(input = getwd(), mrun = NULL, number = NULL, ic = "AICc", colour.par = NULL, save.output = NULL, ...){

  switch (ic,
          "AICc" = {ic.index <- 1},
          "AIC"  = {ic.index <- 2},
          "BIC"  = {ic.index <- 3}
  )
  if(is.character(input)){
    #read in files (either a specific one or all)
    if(is.null(mrun)){
      filenames <- list.files(paste0(input,"/FAMoS-Results/TestedModels/"), pattern="*.rds", full.names=TRUE)
      if(length(filenames) == 0){
        stop("No files in the given folder!")
      }
      store.res <-  readRDS(filenames[1])
      for(i in 2:length(filenames)){
        current.res <- readRDS(filenames[i])
        for(j in 1:ncol(current.res)){
          if(any(colSums(abs(store.res[-c(1:4),] - current.res[-c(1:4),j])) == 0)){
            get.index <- which(colSums(abs(store.res[-c(1:4),] - current.res[-c(1:4),j])) == 0)
            if(store.res[ic.index, get.index] >= current.res[ic.index,j]){
              store.res[ic.index, get.index] <-  current.res[ic.index,j]
            }
          }else{
            store.res <- cbind(store.res, current.res[,j])
          }
        }

      }
      mt <- store.res
    }else{
      if(file.exists(paste0(input,"/FAMoS-Results/TestedModels/TestedModels",mrun,".rds")) == FALSE){
        stop("The specified file does not exist!")
      }
      mt <- readRDS(paste0(input,"/FAMoS-Results/TestedModels/TestedModels",mrun,".rds"))
    }
  }else if(is.matrix(input)){
    mt <- input
  }else{
    stop("Input needs to be either a directory path or a matrix.")
  }
  #excluding failed runs
  if(length(which(is.na(mt[1,]))) > 0){
    mt <- mt[,-c(which(is.na(mt[1,])))]
  }

  mt <- mt[,order(mt[ic.index,])]

  #reorder the input files according to the AICc
  aicc <- mt[ic.index,]




  #reduce the number of depicted models if specified
  if(is.null(number) == FALSE){
    if(number > ncol(mt)){
      stop("'number' is larger than the number of models available.")
    }
    aicc <- aicc[1:number]
    mt <- mt[,1:number]
  }

  #save output file if specified
  if(is.null(save.output) == FALSE){
    grDevices::pdf(file = save.output,
                   width  = 5,
                   height = 5,
                   useDingbats = F)
  }

  row.colours <- rep("black", length(aicc))

  #add color for the parameter if specified
  if(is.null(colour.par) == FALSE){
    if(is.element(colour.par, rownames(mt)) == FALSE){
      stop("The specified parameter is no model parameter.")
    }else{
      row.number <- which(rownames(mt) == colour.par)
      row.parms <- which(mt[row.number,] != 0)
      row.colours[row.parms] <- "red"
    }
  }

  graphics::par(mfrow = c(1,1), mai = c(1,0.9,0.2,0.2))

  #plot DeltaAICc in order
  do.call(graphics::barplot, c(list(height = as.numeric(aicc),
                                    col = row.colours,
                                    names.arg = as.character(1:length(aicc)),
                                    xlab = "model number",
                                    ylab = ic,
                                    cex.axis = 0.7,
                                    cex.names = 0.7,
                                    border = row.colours),
                            list(...)))

  #include legend if results are plotted with respect to a model parameter
  if(is.null(colour.par) == FALSE){
    graphics::legend("topleft",
                     fill = c("black", "red"),
                     legend = c(paste0("Rate ", colour.par, " not included"),
                                paste0("Rate ", colour.par, " included")), cex = 0.6)
  }

  if(is.null(save.output) == FALSE){
    grDevices::dev.off()

  }
  return(mt)
}
