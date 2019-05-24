#' Combine Fitted and Non-fitted Parameters
#'
#' Combines fitted and non-fitted parameters into a single vector, taking into account the specified default values.
#' @param fit.par A named vector containing all parameters that are supposed to be fitted.
#' @param all.names A vector containing the names of all parameters (fitted and non-fitted).
#' @param default.val A named list containing the values that the non-fitted parameters should take. If NULL, all non-fitted parameters will be set to zero. Default values can be either given by a numeric value or by the name of the corresponding parameter the value should be inherited from (NOTE: If a string is supplied, the corresponding parameter entry has to contain a numeric value). Default to NULL.
#' @return A named vector containing the elements of \code{fit.par} and the non-fitted parameters, in the order given by \code{all.names}. The non-fitted parameters are determined by the remaining names in \code{all.names} and their values are set according to \code{default.val}.
#' @export
#' @examples
#' #set parameters, names and default values
#' fits <- c(p1 = 3, p4 = -2)
#' par.names <- c("p1", "p2", "p3", "p4", "p5")
#' defaults <- list(p1 = 4, p2 = 10, p3 = "p1", p4 = 0, p5 = "p4")
#'
#' #combine the parameters in different ways
#' combine.par(fit.par = fits, all.names = par.names)
#' combine.par(fit.par = fits, all.names = par.names, default.val = defaults)
#'
combine.par <- function(fit.par, all.names, default.val = NULL){
  if(length(which(all.names %in% names(fit.par))) != length(fit.par)){
    stop("The names of fit.par have to be included in all.names!")
  }
  if(is.null(default.val) == FALSE && is.list(default.val) == FALSE){
    stop("default.val has to be either NULL or a named list")
  }
  all.par <- rep(0, length(all.names))
  names(all.par) <- all.names
  all.par[all.names %in% names(fit.par)] <- fit.par
  if(is.null(default.val)){
    return(all.par)
  }else{
    get.index <- which((all.names %in% names(fit.par)) == FALSE)
    
    for(k in get.index){
      #if numeric set to the corresponding value
      if(is.numeric(default.val[[k]])){
        all.par[k] <- default.val[[k]]
      }else if(is.character(default.val[[k]])){#if character find the corresponding value
        #check if the corresponding entry is part of the fit vector
        if(is.element(default.val[[k]], names(fit.par))){
          all.par[k] <- fit.par[which(names(fit.par) == default.val[[k]])]
        }else{#if not take the value specified in nofit_zero
          all.par[k] <- default.val[[which(names(default.val) == default.val[[k]])]]
        }
        
      }
    }
    return(all.par)
  }
}
