#' Generate Random Starting Model
#'
#' Generates a random starting model (if specified by the user in \code{\link{famos}}), taking into account the critical conditions and the parameters which should not be fitted.
#' @param number.par The number of total parameters available.
#' @param crit.parms A list containing vectors which specify the critical parameter sets. Needs to be given by index and not by name (for conversion see \code{\link{set.crit.parms}}).
#' @param no.fit A vector containing the indices of the parameters which are not to be fitted.
#' @return A vector containing the parameter indices of the random model.
#' @export
#' @examples
#' #set critical conditions
#' crits <- list(c(1,2,3), c(4,5))
#' #generate random model
#' random.init.model(number.par = 20)
#' random.init.model(number.par = 20, crit.parms = crits)
random.init.model <- function(number.par, crit.parms = NULL, no.fit = NULL){

  init.mod <- c()
  #grab one parameter from each critical set randomly
  if(length(crit.parms) > 0) {
    for(i in 1:length(crit.parms)){
      if(length(crit.parms[[i]]) > 1){
        init.mod <- unique(c(init.mod, sample(x = crit.parms[[i]], size = 1)))
      }else{
        init.mod <- unique(c(init.mod, crit.parms[[i]]))
      }
    }
  }

  samp.vec <- 1:number.par
  if(length(init.mod) == 0 && length(no.fit) == 0) {
    init.mod <- sample(x = samp.vec,
                       size = sample(1:number.par, 1))
  } else {
    init.mod <- unique(c(init.mod, sample(x = samp.vec[-c(init.mod, no.fit)],
                                          size = sample(1:length(samp.vec[-c(init.mod, no.fit)]), 1))))
  }


  return(init.mod[order(init.mod)])
}

