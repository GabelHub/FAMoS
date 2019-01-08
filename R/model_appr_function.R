#' Test for Model Appropriateness
#'
#' Tests if a model is violating specified critical conditions.
#' @param current.parms A vector containing the indices of the current model.
#' @param critical.parms A list containing vectors which specify the critical parameter sets. Needs to be given by index and not by name (for conversion see \code{\link{set.crit.parms}}).
#' @param do.not.fit A vector containing the indices of the parameters that are not to be fitted.
#' @return TRUE, if the model is appropriate, FALSE, if it violates the specified critical conditions.
#' @export
#' @examples
#' #define the models to be checked
#' model1 <- c(1,2,5)
#' model2 <- c(1,4,5)
#' #define the critical conditions
#' crits <- list(c(2,3))
#'
#' #test the models
#' model.appr(current.parms = model1, critical.parms = crits)
#' model.appr(current.parms = model2, critical.parms = crits)

model.appr <- function(current.parms, critical.parms, do.not.fit = NULL){
  if(is.null(do.not.fit) == FALSE){
    if(sum(is.element(do.not.fit, current.parms))> 0){
      return(FALSE)
    }
  }
  if(length(critical.parms) == 0){
    return(TRUE)
  }
  model.valid <- TRUE
  #cycle through all the list entries
  for(i in 1:length(critical.parms)){
    #get list entry
    test.set <- critical.parms[[i]]
    #test if intersection is empty
    if(length(intersect(current.parms, test.set)) == 0){
      #print(i)
      model.valid <- FALSE
      return(FALSE)
    }
  }
  #return if model is valid or not
  return(model.valid)
  
}
