#' Plot Model selection Criteria
#'
#' Plots the selection criteria of the tested models in ascending order.
#' @param input Either a string containing the directory which holds the "FAMoS-Results" folder or a matrix containing the tested models along with the respective selection criteria. Default to \code{getwd()}.
#' @param mrun A string giving the number of the corresponding FAMoS run, e.g "004". If NULL (default), all FAMoS runs in the folder will be used for evaluation.
#' @param number Specifies the number of models that will be plotted. If NULL (default), all tested models will be used for plotting.
#' @param colour.par The name of a model parameter. All models containing this parameter will be coloured red. Default to NULL.
#' @param save.output A string containing the location and name under which the figure should be saved (format is .pdf). Default to NULL.
#' @param ... Additional parameters that will be passed on to \code{\link{barplot}}.
#' @return Barplot showing the ordered selection criteria of the tested models. Also returns a data frame containing each unique tested model with its best selection criteria.
#' @export
#' @examples
#' #plot the selection criteria
#' sc.order(input = famos.run)
#' sc.order(input = famos.run, colour.par = "p1")

sc.order <- function(input = getwd(), mrun = NULL, number = NULL, colour.par = NULL, save.output = NULL, ...){
  
  old.par <- graphics::par("mai")
  on.exit(graphics::par(mai = old.par))
  
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
          if(sum(!is.finite(current.res[1,])) > 0){
            stop(paste0("File\n ", filenames[i], "\n is corrupt!"))
          }
        
          if(any(colSums(abs(store.res[-c(1:4),] - current.res[-c(1:4),j])) == 0)){
            get.index <- which(colSums(abs(store.res[-c(1:4),] - current.res[-c(1:4),j])) == 0)
            get.index <- get.index[which.min(store.res[1,get.index])[1]]
            if(store.res[1, get.index] >= current.res[1,j]){
              store.res[1, get.index] <-  current.res[1,j]
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
  
  mt <- mt[,order(mt[1,])]
  
  #reorder the input files according to the scv
  scv <- mt[1,]
  
  
  
  
  #reduce the number of depicted models if specified
  if(is.null(number) == FALSE){
    if(number > ncol(mt)){
      stop("'number' is larger than the number of models available.")
    }
    scv <- scv[1:number]
    mt <- mt[,1:number]
  }
  
  #save output file if specified
  if(is.null(save.output) == FALSE){
    grDevices::pdf(file = save.output,
                   width  = 5,
                   height = 5,
                   useDingbats = F)
  }
  
  row.colours <- rep("black", length(scv))
  
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
  
  #log graphic scale if appropriate
  if(min(scv) > 0 && (min(scv)/max(scv)) < 3*10^-2 && 
     !is.element("log", names(list(...)))){
    graphics.list <- c(list(...), list(log = "y"))
  }else{
    graphics.list <- list(...)
  }
  
  graphics::par(mai = c(1,0.9,0.4,0.2))
  
  #plot scv in order
  do.call(graphics::barplot, c(list(height = as.numeric(scv),
                                    col = row.colours,
                                    names.arg = as.character(1:length(scv)),
                                    xlab = "model number",
                                    ylab = "SC value",
                                    cex.axis = 0.7,
                                    cex.names = 0.7,
                                    border = row.colours,
                                    main = "Model comparison"),
                               graphics.list))
  
  #include legend if results are plotted with respect to a model parameter
  if(is.null(colour.par) == FALSE){
    graphics::legend("topleft",
                     fill = c("black", "red"),
                     legend = c(paste0(colour.par, " not included"),
                                paste0(colour.par, " included")), cex = 0.6)
  }
  
  if(is.null(save.output) == FALSE){
    grDevices::dev.off()
    
  }
  return(mt)
}