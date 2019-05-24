#' Retrieve Model Results
#'
#' Returns the results of a specified model that was tested by \code{\link{famos}}.
#' @param model Either the binary number of the model as a string (e.g. "011001"), a named vector containing the names the names of the fitted parameters or a vector containing ones and zeroes to indicate which model parameter were fitted. If the names of the fitted parameters are supplied, \code{all.names} must be specified as well.
#' @param homedir  A string giving the directory in which the result folders are found. This is the same directory in which \code{\link{famos}} was run.
#' @param all.names A vector containing the names of all parameters.
#'
#' @return A vector containing the calculated selection criteria and estimated parameters of the specified model.
#' @export
#'
#' @examples
#'
#' #setting data
#' x.values <- 1:7
#' y.values <-  3^2 * x.values^2 - exp(2 * x.values)
#'
#' #define initial conditions and corresponding test function
#' inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
#'
#' cost_function <- function(parms, x.vals, y.vals){
#'  if(max(abs(parms)) > 5){
#'    return(NA)
#'  }
#'  with(as.list(c(parms)), {
#'    res <- p1*4 + p2^2*x.vals^2 + p3*sin(x.vals) + p4*x.vals - exp(p5*x.vals)
#'    diff <- sum((res - y.vals)^2)
#'  })
#' }
#'
#'
#' #perform model selection
#' res <- famos(init.par = inits,
#'             fit.fn = cost_function,
#'             nr.of.data = length(y.values),
#'             homedir = tempdir(),
#'             init.model.type = c("p2", "p3"),
#'             optim.runs = 1,
#'             x.vals = x.values,
#'             y.vals = y.values)
#'
#' #get results
#' retrieve.results(model = "01110", homedir = tempdir())
#' retrieve.results(model = c("p2", "p3", "p4"), homedir = tempdir(),
#'                  all.names = c("p1","p2", "p3", "p4", "p5"))
#' retrieve.results(model = c(0,1,1,1,0), homedir = tempdir())
#'
#' #delete tempdir
#' unlink(paste0(tempdir(),"/FAMoS-Results"), recursive = TRUE)
retrieve.results <- function(model, homedir = getwd(), all.names = NULL){
  options(warn = -1)
  if(is.numeric(model)){
    if(file.exists(paste0(homedir, "/FAMoS-Results/Fits/Model",  paste(model, collapse=""),".rds"))){
      res <- readRDS(paste0(homedir, "/FAMoS-Results/Fits/Model",  paste(model, collapse=""),".rds"))
    }else{
      stop("The specified file does not exist.")
    }
  }else if(is.character(model) && sum(is.na(as.numeric(model))) == 0){
    if(file.exists(paste0(homedir, "/FAMoS-Results/Fits/Model", model,".rds"))){
      res <- readRDS(paste0(homedir, "/FAMoS-Results/Fits/Model", model,".rds"))
    }else{
      stop("The specified file does not exist.")
    }
  }else if(is.character(model)){
    if(is.null(all.names)){
      stop("Supply all parameter names.")
    }else{
      res.vec <- rep(0, length(all.names))
      res.vec[all.names %in% model] <- 1
      if(file.exists(paste0(homedir, "/FAMoS-Results/Fits/Model",  paste(res.vec, collapse=""),".rds"))){
        res <- readRDS(paste0(homedir, "/FAMoS-Results/Fits/Model",  paste(res.vec, collapse=""),".rds"))
      }else{
        stop("The specified file does not exist.")
      }
    }
  }else{
    stop("Supply a correct model definition.")
  }
  options(warn = 0)
  return(res)
}
