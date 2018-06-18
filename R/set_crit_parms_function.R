#' Convert Critical and Swap Sets
#'
#' Converts the user-specified list containing the names of the critical or swap parameters into a format that is more convenient for calculations.
#' @param critical.parameters A list containing vectors which specify either critical parameter sets or swap parameter sets (see Details).
#' @param all.names A vector containing the names of all parameters.
#' @return A list containing the indices of the respective critical or swap sets.
#' @details Critical sets are parameters sets, of which at least one per set has to be present in each tested model. If a model violates on of the critical conditions, it will not be fitted (see also \code{\link{model.appr}}). On the other hand, swap parameter sets define which parameters are interchangeable. For more information see \code{\link{famos}}.
#' @export
#' @examples
#' #set critical set and names
#' crits <- list(c("p1", "p2"), c("p5"))
#' par.names <- c("p1", "p2", "p3", "p4", "p5")
#' #convert the critical conditions
#' set.crit.parms(critical.parameters = crits, all.names = par.names)


set.crit.parms <- function(critical.parameters, all.names) {
  if(is.null(critical.parameters)){
    out <- NULL
  }else{
    out <- list()
    for(i in 1:length(critical.parameters)) {
      out <- c(out, list(sapply(unlist(critical.parameters[i]), function(x) which(all.names == x))))
    }
  }
  return(out)
}
