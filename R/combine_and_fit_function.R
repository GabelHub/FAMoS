#' Combine and Fit Parameters
#'
#' Combines fitted and non-fitted parameters and calls the fitting function. Serves as a wrapping function for the user-specified fitting function \code{fit.fn} (see \code{\link{famos}}).
#' @param par A named vector containing all parameters that are supposed to be fitted.
#' @param par.names The names of all parameters
#' @param fit.fn The cost function (see \code{\link{famos}} for more details).
#' @param binary A vector containing zeroes and ones. Zero indicates that the corresponding parameter is not fitted.
#' @param default.val A named list containing the values that the non-fitted parameters should take. If NULL, all non-fitted parameters will be set to zero. Default values can be either given by a numeric value or by the name of the corresponding parameter the value should be inherited from (NOTE: In this case the corresponding parameter entry has to contain a numeric value). Default to NULL.
#' @param ... Other arguments.
#' @export
#' @return Returns the negative log-likelihood as calculated by the specified cost function
#' @examples
#' #set parameters and cost function
#' fit.par <- c(p1 = 2, p2 = 4)
#' name.par <- c("p1", "p2", "p3")
#' defaults <- list(p1 = 0, p2 = 2, p3 = 4)
#' cost.function <- function(parms){
#'     parms[1] + parms[2] + parms[3]
#' }
#'
#' #call combine.and.fit
#' combine.and.fit(par = fit.par, par.names = name.par, fit.fn = cost.function)
#' combine.and.fit(par = fit.par, par.names = name.par, fit.fn = cost.function, default.val = defaults)
combine.and.fit <- function(par, par.names, fit.fn, binary = NULL, default.val = NULL, ...) {
  
  dots <- list(...)
  if(!is.null(binary)){
    names(par) <- par.names[which(binary == 1)]
  }
  # combine fitted and non-fitted parameters
  total.par  <- combine.par(fit.par = par, all.names = par.names, default.val = default.val)
  
  # call user defined function while passing on user defined variables
  if(is.element("binary", names(formals(fit.fn)))){
    diff <- R.utils::doCall(fit.fn, args = c(list(parms = total.par, binary = binary), dots))
  }else{
    diff <- R.utils::doCall(fit.fn, args = c(list(parms = total.par), dots))
  }
  
  return(diff)
}