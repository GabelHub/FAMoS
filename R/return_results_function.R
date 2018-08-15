#' Return Final Results
#'
#' Returns the results of one FAMoS run. Includes the best parameter sets and corresponding information criterion.
#' @param homedir A string giving the directory in which the result folders are found. This is the same directory in which \code{\link{famos}} was run.
#' @param mrun The number of the FAMoS run that is to be evaluated. Must be a three digit string in the form of '001'. Alternatively, supplying 'best' will return the best result that is found over several FAMoS runs.
#' @param ic The information criterion used. Default is 'AICc". Other options are 'AIC' and 'BIC'.
#' @return A list containing the following elements:
#' \describe{
#'   \item{IC}{The value of the information criterion of the best model.}
#'   \item{par}{The values of the fitted parameter vector corresponding to the best model.}
#'   \item{IC.type}{The type of information criterion used.}
#'   \item{binary}{The binary information of the best model.}
#'   \item{vector}{Vector indicating which parameters were fitted in the best model.}
#'   \item{total.models.tested}{The total number of different models that were analysed. May include repeats.}
#'   \item{mrun}{The number of the current FAMoS run}
#'   \item{initial.mode}{The first model evaluated by the FAMoS run}
#' }
#' @export
#' @examples
#' future::plan(future::sequential)
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
#'            fit.fn = cost_function,
#'            nr.of.data = length(y.values),
#'            homedir = getwd(),
#'            method = "forward",
#'            init.model.type = c("p2", "p3"),
#'            optim.runs = 1,
#'            x.vals = x.values,
#'            y.vals = y.values)
#'
#' #get results
#' return.results(homedir = getwd(), mrun = res$mrun)
return.results <- function(homedir, mrun, ic = "AICc"){
  #get the according information criterion
  switch (ic,
          "AICc" = {ic.index <- 1},
          "AIC"  = {ic.index <- 2},
          "BIC"  = {ic.index <- 3}
  )

  if(mrun == "best"){
    best.ic <- Inf
    filenames <- list.files(paste0(homedir,"/FAMoS-Results/BestModel"), pattern="*.rds", full.names=TRUE)
    for(i in 1:length(filenames)){
      bm <- readRDS(filenames[[i]])
      if(bm[ic.index] < best.ic){
        best.ic <- bm[ic.index]
        mrun <- gsub(paste0(homedir,"/FAMoS-Results/BestModel/BestModel"), "",filenames[[i]])
        mrun <- gsub(".rds", "", mrun)
      }
    }
  }
  cat(paste0("FAMoS run ", mrun), sep = "\n")
  #get information criterion of best model
  bm <- readRDS(paste0(homedir, "/FAMoS-Results/BestModel/BestModel", mrun,".rds"))
  cat(paste0(ic, " of best model: ", round(bm[ic], 2)), sep = "\n")
  #get best model
  mt <- readRDS(paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels", mrun,".rds"))
  min.index <- as.numeric(which(mt[ic.index,] == min(mt[ic.index,], na.rm = TRUE)))

  cat(paste0("Best model (binary): ", paste(mt[-c(1:4), min.index], collapse="")), sep = "\n")

  cat("Best model (vector):", sep = "\n")
  print(mt[-c(1:4), min.index])
  cat("Estimated parameter values:", sep = "\n")
  print(bm[-c(1:3)])
  #save output as list
  output <- list(IC = round(bm[ic], 2),
                 par = bm[-c(1:3)],
                 IC.type = ic,
                 binary = paste(mt[-c(1:4), min.index], collapse=""),
                 vector = mt[-c(1:4), min.index],
                 total.models.tested = ncol(mt),
                 mrun = mrun,
                 initial.model = mt[-c(1:4),1])

  return(output)
}
