#' Control Function for Optim
#'
#' This function is designed to use the built-in option \code{parscale} in \code{\link{optim}} with absolute scaling values.
#'
#' @param par A vector containing the values of the original parameters.
#' @param scale A vector containing the corresponding absolute scaling values that will be used during the first steps in \code{\link{optim}}.
#' @param fix Integer containing the index of the parameter that will be scaled by 10\% of its original value, meaning the corresponding entry in scale will be overwritten (\code{parscale} in \code{\link{optim}} needs one value like this). Default to 1.
#' @param correction Used for scaling newly added parameter values by their original scale as specified in \code{init.par} in \code{\link{famos}}. Default to NULL.
#'
#' @return A vector that can be used to scale \code{parscale} in \code{\link{optim}} accordingly.
#' @export
#'
#' @examples
#' test.func<-function(x){
#' print(x)
#' 3*x[1]+4*x[2]
#' }
#'
#' pars <- c(1, 1000, 10)
#' scaling <- c(0.1, 3000, 10)
#'
#' p.scale <- parscale.famos(par = pars, scale = scaling)
#'
#' optim(par = pars, fn = test.func, control = list(maxit = 10, parscale = p.scale, trace = TRUE))
#'
parscale.famos <- function(par, scale, fix = 1, correction = NULL){
  #check if length of scale is equal to par
  if(length(par) !=  length(scale)){
    stop("parscale.famos has parameter and scaling vectors of different sizes.")
  }
  
  if(any(scale == 0)){
    if(!is.null(correction)){
      scale[scale == 0] <- correction[which(scale == 0)]
    }else{
      scale[scale == 0] <- 1
    }
  }
  #parscale fixes the larged par/parscale value to deviate only 10 percent, others can then vary
  #get fixed and maximal value
  fix.value <- par[fix]
  if(fix.value == 0){
    fix.value <- 1
  }
  
  max.value <- abs(par[which.max(abs(par))[1]])
  if(max.value == 0){
    print("Warning: Vector contains only zeroes. Scaling is set to a default of 1.")
    max.value <- 1
  }
  
  #fill in scaling vector
  par.scale <- scale/(0.1 * fix.value * max.value)
  par.scale[fix] <- 1 / max.value
  
  return(abs(par.scale))
}
