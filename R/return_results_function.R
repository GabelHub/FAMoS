#' Return Final Results
#'
#' Returns the results of one FAMoS run. Includes the best parameter sets and corresponding selection criterion.
#' @param homedir A string giving the directory in which the result folders are found. This is the same directory in which \code{\link{famos}} was run.
#' @param mrun The number of the FAMoS run that is to be evaluated. Must be a three digit string in the form of '001'. Alternatively, supplying 'best' will return the best result that is found over several FAMoS runs.
#' @return A list containing the following elements:
#' \describe{
#'   \item{SCV}{The value of the selection criterion of the best model.}
#'   \item{par}{The values of the fitted parameter vector corresponding to the best model.}
#'   \item{binary}{The binary information of the best model.}
#'   \item{vector}{Vector indicating which parameters were fitted in the best model.}
#'   \item{total.models.tested}{The total number of different models that were analysed. May include repeats.}
#'   \item{mrun}{The number of the current FAMoS run.}
#'   \item{initial.mode}{The first model evaluated by the FAMoS run.}
#' }
#' @export
#' @examples
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
#'            homedir = tempdir(),
#'            init.model.type = c("p2", "p3"),
#'            optim.runs = 1,
#'            x.vals = x.values,
#'            y.vals = y.values)
#'
#' #get results
#' return.results(homedir = tempdir(), mrun = res$mrun)
#'
#' #delete tempdir
#' unlink(paste0(tempdir(),"/FAMoS-Results"), recursive = TRUE)
return.results <- function(homedir, mrun){

  mrun.old <- mrun
  best.sc <- Inf
  filenames <- list.files(paste0(homedir,"/FAMoS-Results/BestModel"), pattern="*.rds", full.names=TRUE)
  for(i in 1:length(filenames)){
    bm <- readRDS(filenames[[i]])
    if(bm[1] < best.sc){
      best.sc <- bm[1]
      if(mrun.old == "best"){
        mrun <- gsub(paste0(homedir,"/FAMoS-Results/BestModel/BestModel"), "",filenames[[i]])
        mrun <- gsub(".rds", "", mrun)
      }
    }
  }

  if(mrun.old == "best"){
    #get information criterion of best model
    cat(paste0("Selection criterion value of best model over all runs: ", round(best.sc, 2)), sep = "\n")
    #get best model
    filenames <- list.files(paste0(homedir,"/FAMoS-Results/TestedModels"), pattern="*.rds", full.names=TRUE)
    mnumber <- 0
    for(i in 1:length(filenames)){
      mnumber <- mnumber + ncol(readRDS(filenames[i]))
    }
    mt <- readRDS(paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels", mrun,".rds"))
    min.index <- as.numeric(which(mt[1,] == min(mt[1,], na.rm = TRUE)))[1]
    cat(paste0("Total number of tested models (might include duplicates): ",mnumber, sep = "\n"))
    cat(paste0("Best model (binary): ", paste(mt[-c(1:2), min.index], collapse="")), sep = "\n")

    cat("Selected parameters:", sep = "\n")
    best.m <- mt[-c(1:2), min.index]
    print(names(best.m[which(best.m == 1)]))
    cat("Estimated parameter values:", sep = "\n")
    print(bm[-1])
    bm.out <- bm[-1]
    #save output as list
    output <- list(SCV = round(bm[1], 2),
                   par = bm.out[which(best.m == 1)],
                   binary = paste(mt[-c(1:2), min.index], collapse=""),
                   vector = mt[-c(1:2), min.index],
                   total.models.tested = ncol(mt),
                   mrun = mrun,
                   initial.model = mt[-c(1:2),1])

    return(output)
  }else{
    cat(paste0("\nFAMoS run ", mrun), sep = "\n")
    #get information criterion of best model
    bm <- readRDS(paste0(homedir, "/FAMoS-Results/BestModel/BestModel", mrun,".rds"))
    cat(paste0("Selection criterion value of best model in this run: ", round(bm[1], 2)), sep = "\n")
    cat(paste0("Selection criterion value of best model over all runs: ", round(best.sc, 2)), sep = "\n")
    #get best model
    mt <- readRDS(paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels", mrun,".rds"))
    min.index <- as.numeric(which(mt[1,] == min(mt[1,], na.rm = TRUE)))[1]
    cat(paste0("Number of tested models during this run: ", ncol(mt)), sep = "\n")
    cat(paste0("Best model (binary): ", paste(mt[-c(1:2), min.index], collapse="")), sep = "\n")

    cat("Selected parameters:", sep = "\n")
    best.m <- mt[-c(1:2), min.index]
    print(names(best.m[which(best.m == 1)]))
    cat("Estimated parameter values:", sep = "\n")
    print(bm[-1])
    bm.out <- bm[-1]
    #save output as list
    output <- list(SCV = round(bm[1], 2),
                   par = bm.out[which(best.m == 1)],
                   binary = paste(mt[-c(1:2), min.index], collapse=""),
                   vector = mt[-c(1:2), min.index],
                   total.models.tested = ncol(mt),
                   mrun = mrun,
                   initial.model = mt[-c(1:2),1])

    return(output)
  }

}
