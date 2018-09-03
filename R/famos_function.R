#' Automated Model Selection
#'
#' Given a vector containing all parameters of interest and a cost function, the \code{FAMoS} looks for the most appropriate subset model to describe the given data.
#' @param init.par A named vector containing the initial parameter values.
#' @param fit.fn A cost function. Has to take the complete parameter vector as an input (needs to be names \code{parms}) and must return the corresponding negative log-likelihood (-2LL, see Burnham and Anderson 2002). The binary vector, containing the information which parameters are currently fitted, can also be used by taking \code{binary} as an additional function input argument.
#' @param nr.of.data The number of data points used for fitting.
#' @param homedir The directory to which the results should be saved to.
#' @param do.not.fit The names of the parameters that are not supposed to be fitted. Default is NULL.
#' @param method The starting method of the FAMoS. Options are "forward" (forward search), "backward" (backward elimination) and "swap" (only if \code{critical.parameters} or \code{swap.parameters} are supplied). Methods are adaptively changed over each iteration of the FAMoS. Default to "forward".
#' @param init.model.type The starting model. Options are "global" (starts with the complete model) or "random" (creates a randomly sampled starting model). Alternatively, a specific model can be used by giving the corresponding names of the parameters one wants to start with. Default to "random".
#' @param refit If TRUE, previously tested models will be tested again. Default to FALSE.
#' @param optim.runs The number of times that each model will be fitted by \code{\link{optim}}. Default to 5.
#' @param information.criterion The information criterion the model selection will be based on. Options are "AICc", "AIC" and "BIC". Default to "AICc".
#' @param default.val A named list containing the values that the non-fitted parameters should take. If NULL, all non-fitted parameters will be set to zero. Default values can be either given by a numeric value or by the name of the corresponding parameter the value should be inherited from (NOTE: In this case the corresponding parameter entry has to contain a numeric value). Default to NULL.
#' @param swap.parameters A list specifying which parameters are interchangeable. Each swap set is given as a vector containing the names of the respective parameters. Default to NULL.
#' @param critical.parameters A list specifying sets of critical parameters. Critical sets are parameters sets, of which at least one parameter per set has to be present in each tested model. Default to NULL.
#' @param random.borders The ranges from which the random initial parameter conditions for all \code{optim.runs} larger than one are sampled. Can be either given as a vector containing the relative deviations for all parameters or as a matrix containing in its first column the lower and in its second column the upper border values. Parameters are uniformly sampled based on \code{\link{runif}}. Default to 1 (100\% deviation of all parameters). Alternatively, functions such as \code{\link{rnorm}}, \code{\link{rchisq}}, etc. can be used if the additional arguments are passed along as well.
#' @param control.optim Control parameters passed along to \code{optim}. For more details, see \code{\link{optim}}.
#' @param parscale.pars Logical. If TRUE (default), the \code{parscale} option will be used when fitting with \code{\link{optim}}. This is helpful, if the parameter values are on different scales.
#' @param con.tol The absolute convergence tolerance of each fitting run (see Details). Default is set to 0.1.
#' @param save.performance Logical. If TRUE, the performance of \code{FAMoS} will be evaluated in each iteration via \code{\link{famos.performance}}, which will save the corresponding plot into the folder "FAMoS-Results/Figures/" (starting from iteration 3) and simultaneously show it on screen. Default to TRUE.
#' @param future.off Logical. If TRUE, FAMoS runs without the use of \code{futures}. Useful for debugging.
#' @param ... Other arguments that will be passed along to \code{\link{future}}, \code{\link{optim}} or the user-specified cost function \code{fit.fn}.
#' @details In each iteration, the FAMoS finds all neighbouring models based on the current model and method, and subsequently tests them. If one of the tested models performs better than the current model, the model, but not the method, will be updated. Otherwise, the method, but not the model, will be adaptively changed, depending on the previously used methods.
#' @export
#' @return A list containing the following elements:
#' \describe{
#'   \item{IC}{The value of the information criterion of the best model.}
#'   \item{par}{The values of the fitted parameter vector corresponding to the best model.}
#'   \item{IC.type}{The type of information criterion used.}
#'   \item{binary}{The binary information of the best model.}
#'   \item{vector}{Vector indicating which parameters were fitted in the best model.}
#'   \item{total.models.tested}{The total number of different models that were analysed. May include repeats.}
#'    \item{mrun}{The number of the current FAMoS run.}
#'   \item{initial.model}{The first model evaluated by the FAMoS run.}
#'   }
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
#'    res <- 4*p1 + p2^2*x.vals^2 + p3*sin(x.vals) + p4*x.vals - exp(p5*x.vals)
#'    diff <- sum((res - y.vals)^2)
#'  })
#' }
#'
#' #set swap set
#' swaps <- list(c("p1", "p5"))
#'
#' #perform model selection
#' res <- famos(init.par = inits,
#'             fit.fn = cost_function,
#'             nr.of.data = length(y.values),
#'             homedir = getwd(),
#'             method = "swap",
#'             swap.parameters = swaps,
#'             init.model.type = c("p1", "p3"),
#'             optim.runs = 1,
#'             x.vals = x.values,
#'             y.vals = y.values)

famos <- function(init.par,
                  fit.fn,
                  nr.of.data,
                  homedir = getwd(),
                  do.not.fit = NULL,
                  method = "forward",
                  init.model.type = "random",
                  refit = FALSE,
                  optim.runs = 5,
                  information.criterion = "AICc",
                  default.val = NULL,
                  swap.parameters = NULL,
                  critical.parameters = NULL,
                  random.borders = 1,
                  control.optim = list(maxit = 1000),
                  parscale.pars = TRUE,
                  con.tol = 0.1,
                  save.performance = TRUE,
                  future.off = FALSE,
                  ...
) {
  #test the appropriateness of parameters
  if(is.vector(init.par) == FALSE || is.null(names(init.par))){
    stop("Initial parameters must be given as named vector.")
  }
  if(is.null(do.not.fit) == FALSE && is.character(do.not.fit) == FALSE){
    stop("do.not.fit must be a character vector.")
  }
  if(is.character(init.model.type) == FALSE){
    stop("init.model.type must be a character vector.")
  }
  if(is.null(default.val) == FALSE && is.list(default.val) == FALSE){
    stop("default.val must be a list.")
  }
  if(is.null(critical.parameters) == FALSE && is.list(critical.parameters) == FALSE){
    stop("critical.parameters must be a list.")
  }
  if(is.null(swap.parameters) == FALSE && is.list(swap.parameters) == FALSE){
    stop("swap.parameters must be a list.")
  }
  if(information.criterion == "AICc" && nr.of.data <= (length(init.par) + 1) ){
    stop("Due to insufficient data the AICc cannot be calculated. Choose 'AIC' or 'BIC' instead.")
  }
  if(method != "forward" && method != "backward" && method != "swap"){
    stop("Incorrect method name! Use either 'forward', 'backward' or 'swap'.")
  }
  if(method == "swap" && is.null(swap.parameters) && is.null(critical.parameters) ){
    stop("Please supply either a swap or a critical parameter set or change the initial search method.")
  }
  if(future.off == F && class(try(future::plan()))[1] == "try-error"){
    stop("Please specify a plan from the future-package. See ?future::plan for more information.")
  }
  if(is.vector(random.borders) == FALSE && is.matrix(random.borders) == FALSE){
    stop("random.borders must either be a number, a vector or a matrix.")
  }
  cat("\nInitializing...", sep = "\n")

  #set starting time
  start <- Sys.time()
  #create FAMoS directory
  cat("Create FAMoS directory...", sep = "\n")
  make.directories(homedir)

  #get mrun for unique labelling of this run
  mrun_old <- list.files(path = paste0(homedir, "/FAMoS-Results/TestedModels/"),
                         pattern = "TestedModels.*.rds")
  if (length(mrun_old) == 0) {
    mrun <- 1
  } else {
    mrun_old <- as.double(gsub(".rds", "", gsub("TestedModels", "", mrun_old)))
    mrun <- max(mrun_old) + 1
  }
  mrun <- formatC(mrun, width = 3, format = "d", flag = "0") # width determines maximum amount of runs, 3 = 999, 4 = 9999 and so on
  #mrun <- mrun.x

  cat(paste0("\nAlgorithm run: ", mrun), sep = "\n")

  # get all parameter names
  all.names <- names(init.par)

  #prepare critical and swap parameters for algorithm
  if(length(critical.parameters) == 0) {
    #critical.parameters <- list(all.names)
    no.crit <- TRUE
    crit.parms <- list()
  }else{
    no.crit <- FALSE
    crit.parms <- set.crit.parms(critical.parameters = critical.parameters,
                                 all.names = all.names)
  }


  if(length(swap.parameters) == 0){
    no.swap <- TRUE
    swap.parms <- list()
  }else{
    no.swap <- FALSE
    swap.parms <- set.crit.parms(critical.parameters = swap.parameters,
                                 all.names = all.names)
  }

  #get indices of do.not.fit
  if(is.null(do.not.fit) != TRUE){
    do.not.fit <- which( (names(init.par) %in% do.not.fit) == TRUE)
  }

  #set scaling values
  scaling.values <- abs(init.par)
  scaling.values[scaling.values == 0] <- 1

  if(length(init.model.type) == 1 && (init.model.type == "global" || init.model.type == "random")){
    switch (init.model.type,
            "global" = {
              #take the global model
              init.model <- 1:length(init.par)
              if(length(do.not.fit) > 0){
                init.model <- init.model[-c(do.not.fit)]
              }
            },
            "random" = {
              #get a random initial model
              init.model <- random.init.model(number.par = length(init.par),
                                              crit.parms = crit.parms,
                                              no.fit = do.not.fit)
            }
    )
  }else{
    #take the model specified by the user
    init.model <- c()
    for(i in 1:length(init.model.type)) {
      init.model[i] <-  which(all.names == init.model.type[i] )
    }
    init.model <- init.model[order(init.model)]
    #check if model is appropriate
    if(model.appr(current.parms = init.model,
                  critical.parms = crit.parms,
                  do.not.fit = do.not.fit) == FALSE){
      stop("The specified initial model violates critical conditions or the do.not.fit specifications")
    }
  }

  #set initial parameters as starting parameters
  best.par <- init.par

  #define count variable for runs in total (tells you how many different methods were tested)
  model.run <- 1
  #create storage for models tested
  models.tested <- c()
  saveRDS(models.tested, paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels",mrun,".rds"))
  models.per.run <- c()
  #create storage for AICCs for the tested models
  save.AICC <- c()

  # determine which information criterion to use for model comparison and print out
  switch(information.criterion,
         "AICc" = {
           ic <- 1
         }, "AIC" = {
           ic <- 2
         }, "BIC" = {
           ic <- 3
         })
  cat(paste0("Information criterion for model comparison: ", information.criterion), sep = "\n")

  #print if refitting is enabled or not
  if(refit) {
    cat("Refitting enabled.", sep = "\n")
  }else{
    cat("Refitting disabled.", sep = "\n")
  }

  cat(paste0("Starting algorithm with method '", method, "'"), sep = "\n")

  #########################
  ##### MAIN ROUTINE ######
  #########################
  repeat{

    #define model to be used in the first run
    if(model.run == 1){

      cat(paste0("\nFAMoS iteration #", model.run, " - fitting starting model"), sep = "\n")

      pick.model <- init.model
      #save model for next step
      pick.model.prev <- pick.model
      #set fitted parameters to 1 (curr.model is saved later)
      curr.model <- rep(0,length(init.par))
      curr.model[pick.model] <- 1
      curr.model.all <- cbind(c(),curr.model)
      #set history
      previous <- method

    }else{#for all methods besides the first

      cat(paste0("\nFAMoS iteration #", model.run, " - method: ", method), sep = "\n")

      #check if parameters are added (forward method)
      switch(method,"forward" = {#forward####
        #get the length of the total parameter vector
        samp.vec <- 1:length(init.par)
        #get parameters that are currently not in the model
        if(sum(pick.model.prev)==0) {
          parms.left <- samp.vec
        } else {
          parms.left <- samp.vec[-c(pick.model.prev, do.not.fit)]
        }

        #check if the current model is the global model
        if(length(parms.left) == 0){
          method = "backward"
          next
        }

        #create all neighbouring models based on forward search
        curr.model.all <- c()
        for(j in 1:length(parms.left)){
          #print tested parameter
          cat(paste0("Add parameter ", all.names[parms.left[j]]), sep = "\n")

          #get the indices of the currently tested parameter set
          pick.model <- c(parms.left[j], pick.model.prev)

          #get tag of current model (NOTE: I binarised the model ID. Model0110 means that parameter 1 was not included, parameter 2 was included, parameter 3 was included, parameter 4 was not included)
          curr.model <- rep(0,length(init.par))
          curr.model[pick.model] <- 1

          #test if model violates the critical conditions
          if(model.appr(pick.model, crit.parms, do.not.fit = do.not.fit) == FALSE){
            cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
            next
          }
          #check if model has been tested before
          if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
            cat("Combination has been tested before", sep = "\n")
            next
          }
          #if model was neither skipped nor tested before, add to the testing catalogue
          curr.model.all <- cbind(curr.model.all, curr.model)
          #print(curr.model.all)
        }

      }, "backward" = {#backward (backwards search)####
        #get the length of the total parameter vector
        samp.vec <- 1:length(init.par)

        #get parameters that are currently in the model
        parms.left <- pick.model.prev

        #set variable for non-fitted parameter subtractions
        nosub <- c()

        curr.model.all <- c()
        #get all suitable parameter combinations
        for(j in 1:length(parms.left)){
          cat(paste0("Remove parameter ", all.names[parms.left[j]]), sep = "\n")

          # pick.model <- c(sample(x = samp.vec[-pick.model.prev], size = 1), pick.model.prev)
          pick.model <- c(parms.left[-j])


          curr.model <- rep(0,length(init.par))
          curr.model[pick.model] <- 1

          #test if model violates the critical conditions
          if(model.appr(pick.model, crit.parms, do.not.fit = do.not.fit) == FALSE){
            cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
            nosub <- c(nosub, j)
            next
          }
          #check if model has been tested before
          if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
            cat("Combination has been tested before", sep = "\n")
            nosub <- c(nosub, j)
            next
          }

          # add appropriate models to the testing catalogue
          curr.model.all <- cbind(curr.model.all, curr.model)

        }

        if(length(nosub) != 0) {
          parms.left <- parms.left[-nosub]
        }

        if(length(ncol(curr.model.all)) == 0){#if all removable parameters are critical jump to forward
          previous <- method
          method <- "forward"
          model.run <- model.run + 1
          cat("All removable parameters are critical - switch to forward search.", sep = "\n")
          next
        }
      }, "swap" = {#swap ####
        #swap method exchanges parameter values used within the recent best model with those not used

        #get parameter sets for testing
        parm.comb <- c(crit.parms, swap.parms)
        curr.model.all <- c()
        parms.left <- c()

        #for-loop will output all swap combinations within each parameter set
        for(i in 1:length(parm.comb)) {
          if(length(parm.comb[[i]])<=1) {
            next
          }

          p_list <- parm.comb[[i]]

          #save used and unused parameters of current set
          par_1 <- intersect(p_list, pick.model.prev)
          par_0 <- setdiff(p_list, par_1)

          #create all possible combinations between used and unused parameters
          cmb <- expand.grid(on = par_1, off = par_0)
          parms.left <- rbind(parms.left, cmb)

          #create new model for each of those combinations
          if(nrow(cmb) > 0) {
            for(j in 1:nrow(cmb)) {
              curr.model <- rep(0,length(init.par))
              curr.model[pick.model.prev] <- 1
              curr.model[as.numeric(cmb[j,])] <- abs(curr.model[as.numeric(cmb[j,])]-1)

              cat(paste0("Replace ", all.names[parms.left[j,1]], " by ", all.names[parms.left[j,2]], "\n"))
              #check if model has been tested before while refitting is not enabled
              if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
                cat("Combination has been tested before", sep = "\n")
                next
              }

              #test if model violates the critical conditions
              pick.model <- which(curr.model != 0)
              if(model.appr(pick.model, crit.parms,do.not.fit = do.not.fit) == FALSE){
                cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
                next
              }

              # add appropriate models to testing catalogue
              curr.model.all <- cbind(curr.model.all, abs(curr.model))
            }
          }
        }

        # if swap method fails to provide new valid models, the algorithm is being terminated
        if(sum(curr.model.all)==0) {
          cat("swap method does not yield any valid model. FAMoS terminated.", sep = "\n")
          timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
          cat(paste0("Time needed: ",
                     sprintf("%02d:%02d:%02d",
                             timediff %% 86400 %/% 3600,  # hours
                             timediff %% 3600 %/% 60,  # minutes
                             timediff %% 60 %/% 1), # seconds,
                     sep = "\n"))
          final.results <- return.results(homedir, mrun, ic = information.criterion)
          #setwd(old.directory)
          return(final.results)
        }

      }
      )}

    if(is.null(curr.model.all)) {
      cat("All neighbouring models have been tested during this run. Algorithm halted.", sep = "\n")
      timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
      cat(paste0("Time needed: ",
                 sprintf("%02d:%02d:%02d",
                         timediff %% 86400 %/% 3600,  # hours
                         timediff %% 3600 %/% 60,  # minutes
                         timediff %% 60 %/% 1), # seconds,
                 sep = "\n"))
      break
    }

    #check if current model does contain parameters. If not, method is changed to forward since a model with 0 parameters would not fit our data well enough.
    if(sum(curr.model.all) == 0) {
      method <- "forward"
      previous <- "backward"
      cat("Model does not include any parameter. Continue with forward search.", sep = "\n")
      model.run <- model.run + 1
      models.tested <- cbind(models.tested, curr.model.all)
      models.per.run <- cbind(models.per.run, rbind(model.run, curr.model.all))
      save.AICC <- cbind(save.AICC, NA)
      next
    }

    #update the catalogue of tested models
    models.tested <- cbind(models.tested, curr.model.all)
    models.per.run <- cbind(models.per.run, rbind(model.run, curr.model.all))

    timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
    cat(paste0("Time passed since start: ",
               sprintf("%02d:%02d:%02d",
                       timediff %% 86400 %/% 3600,  # hours
                       timediff %% 3600 %/% 60,  # minutes
                       timediff %% 60 %/% 1), # seconds,
               sep = "\n"))

    cat("Job submission:", sep = "\n")
    #Job submission####
    #submit each individual model to the cluster if it hasn't been tested before
    for(j in 1:ncol(curr.model.all)){
      if(file.exists(paste0(homedir, "/FAMoS-Results/Fits/Model",paste(curr.model.all[,j], collapse =""), ".rds")) == FALSE  ||  refit){
        cat(paste0("Job ID for model ", formatC(j, width = 2, format = "d", flag = "0"), " - ", paste(curr.model.all[,j], collapse="")), sep = "\n")

        if(future.off == F){
          assign(paste0("model",j),
                 future::future({
                   base.optim(binary = curr.model.all[,j],
                              parms = best.par,
                              fit.fn = fit.fn,
                              nr.of.data = nr.of.data,
                              homedir = homedir,
                              optim.runs = optim.runs,
                              information.criterion = information.criterion,
                              default.val = default.val,
                              random.borders = random.borders,
                              control.optim = control.optim,
                              parscale.pars = parscale.pars,
                              scaling = scaling.values,
                              con.tol = con.tol,
                              ...)
                 },
                 label = paste0("Model", paste(curr.model.all[,j], collapse ="")),
                 ...
                 )
          )
        }else{
          base.optim(binary = curr.model.all[,j],
                     parms = best.par,
                     fit.fn = fit.fn,
                     nr.of.data = nr.of.data,
                     homedir = homedir,
                     optim.runs = optim.runs,
                     information.criterion = information.criterion,
                     default.val = default.val,
                     random.borders = random.borders,
                     control.optim = control.optim,
                     parscale.pars = parscale.pars,
                     scaling = scaling.values,
                     con.tol = con.tol,
                     ...)
        }


      }else{
        assign(paste0("model",j), "no.refit")
        cat(paste0("Model fit for ", paste(curr.model.all[,j], collapse="")," exists and refitting is not enabled."), sep = "\n")
      }
    }

    if(future.off == FALSE){
      #set looping variable
      waiting <- TRUE
      waited.models <- rep(1,ncol(curr.model.all))
      time.waited <- Sys.time()
      time.passed <- -1
      ticker <- 0

      #check if the job is still running. If the job is not running anymore restart.
      while(waiting == TRUE){
        update.log <- FALSE
        waiting <- FALSE
        #cycle through all models
        for(j in which(waited.models == 1)){

          if(future::resolved(get(paste0("model", j))) == FALSE){
            waiting <- TRUE
          }else{

            #check if output was generated
            if(!file.exists(paste0(homedir,
                                   "/FAMoS-Results/Fits/Model",
                                   paste(curr.model.all[,j], collapse=""),
                                   ".rds"))){
              Sys.sleep(5)
            }

            if(!file.exists(paste0(homedir,
                                   "/FAMoS-Results/Fits/Model",
                                   paste(curr.model.all[,j], collapse=""),
                                   ".rds"))){
              assign(paste0("model",j),
                     future::future({
                       base.optim(binary = curr.model.all[,j],
                                  parms = best.par,
                                  fit.fn = fit.fn,
                                  nr.of.data = nr.of.data,
                                  homedir = homedir,
                                  optim.runs = optim.runs,
                                  information.criterion = information.criterion,
                                  default.val = default.val,
                                  random.borders = random.borders,
                                  control.optim = control.optim,
                                  parscale.pars = parscale.pars,
                                  scaling = scaling.values,
                                  con.tol = con.tol,
                                  ...)
                     },
                     label = paste0("Model", paste(curr.model.all[,j], collapse ="")),
                     ...
                     )
              )
              cat(paste0("Future terminated but no output file was generated. Model ",j," was automatically resubmitted. If this message repeats, check the log file for more information or use 'future.off = TRUE' to debug."), sep = "\n")

              waiting <- TRUE
            }else{
              #update waiting variable
              waiting <- waiting || FALSE
              #update waiting log
              waited.models[j] <- 0
              update.log <- TRUE
            }

          }
        }
        if(waiting == TRUE){
          #print("Waiting ...")
          if(time.passed == -1){
            cat("Waiting for model fits ...", sep = "\n")
          }
          Sys.sleep(5)
        }

        time.passed <- round(difftime(Sys.time(),time.waited, units = "secs")[[1]],2) - ticker*600

        #output the log for the models that is waited for (every 5 min)
        if( (time.passed > 600) ){
          ticker <- ticker + 1
          nr.running <-  length(which(waited.models == 1))

          timediff <- difftime(Sys.time(),time.waited, units = "secs")[[1]]
          cat(paste0("Time spent waiting so far: ",
                     sprintf("%02d:%02d:%02d",
                             timediff %% 86400 %/% 3600,  # hours
                             timediff %% 3600 %/% 60,  # minutes
                             timediff %% 60 %/% 1), # seconds,
                     sep = "\n"))

          if(update.log == TRUE){
            #calculate difference in time
            if(nr.running == ncol(curr.model.all)){
              cat("Waiting for fits of all models ...", sep = "\n")
            }else{
              cat("Waiting for fits of these models:", sep = "\n")
              cat(paste0(which(waited.models == 1)))
              cat("",sep = "\n")
            }
          }
        }

      }
    }
    #read in files
    get.AICC <- c()
    cat("Evaluate results ...", sep = "\n")
    for(j in 1:ncol(curr.model.all)){

      #this checks if the connection to the fit results file can be made and waits if this is not the case
      waiting <- T
      options(warn = -1)
      while(inherits(try(readRDS(paste0(homedir,
                                        "/FAMoS-Results/Fits/Model",
                                        paste(curr.model.all[,j], collapse =""),
                                        ".rds")),
                         silent = T),
                     "try-error")){
        if(waiting) {
          waiting <- F
          cat(paste0("Trying to read in results file of model ",
                     paste(curr.model.all[,j], collapse =""), "..."),
              sep = "\n")
        }
        Sys.sleep(1)
      }
      options(warn=0)

      #waiting is over, read out file
      get.AICC <- cbind(get.AICC,
                        readRDS(paste0(homedir,
                                       "/FAMoS-Results/Fits/Model",
                                       paste(curr.model.all[,j], collapse =""),
                                       ".rds")))
    }

    #save the resulted AICcs

    Aicc <- get.AICC[1:3,]
    save.AICC <- cbind(save.AICC, Aicc)

    #save AICcs with the corresponding models
    saveTestedModels <- rbind(save.AICC, models.per.run)
    row.names(saveTestedModels) <- c("AICc", "AIC", "BIC", "iteration", all.names)
    colnames(saveTestedModels) <- 1:length(colnames(saveTestedModels))
    saveRDS(saveTestedModels, paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels",mrun,".rds"))

    #if more than one model was tested, order them according to their performance
    if(ncol(get.AICC) > 1){
      index.AICC <- which(get.AICC[ic,] == min(get.AICC[ic,]))[1]
      get.AICC <- get.AICC[,order(get.AICC[ic,])]
      get.AICC <- as.numeric(get.AICC[,1])
    } else {
      index.AICC <- 1
      get.AICC <- as.numeric(get.AICC[,1])
    }

    names(get.AICC) <- c("AICc", "AIC", "BIC", all.names)

    #print currently best fit
    curr.AICC <- get.AICC[ic]
    cat(paste0("Best ", information.criterion, " of this run is ",round(curr.AICC,2)), sep = "\n")

    #update if new model is better
    if(model.run == 1){
      old.AICC <- curr.AICC
      pick.model.prev <- which(curr.model.all[,index.AICC] == 1)
      best.par <- get.AICC[-c(1:3)]

      bm.bin <- curr.model.all[,index.AICC]

      saveRDS(get.AICC, paste0(homedir, "/FAMoS-Results/BestModel/BestModel",mrun,".rds"))

    }else{# if it's not the first run, we know what the previous and current method are
      if((curr.AICC) < old.AICC ){#update the model if a better model is found
        old.AICC <- curr.AICC
        pick.model.prev <- which(curr.model.all[,index.AICC] == 1)
        best.par <- get.AICC[-c(1:3)]

        bm.bin <- curr.model.all[,index.AICC]

        #update previous and current method and output everything to the log file
        switch(method, "forward" = {
          previous <- "forward"
          method <- "forward"
          cat(paste0("Parameter ", all.names[parms.left[index.AICC]], " was added"), sep = "\n")
        }, "backward" = {
          previous <- "backward"
          method <- "backward"
          cat(paste0("Parameter ", all.names[parms.left[index.AICC]], " was removed"), sep = "\n")
        }, "swap" = {
          previous <- "swap"
          method <- "forward"
          cat(paste0("Parameter ", all.names[parms.left[index.AICC,2]], " was exchanged for ", all.names[parms.left[index.AICC,1]]), sep = "\n")
          cat(paste0("Switch to method '", method, "'"), sep = "\n")
        }
        )

        #save best AICc
        saveRDS(get.AICC, paste0(homedir, "/FAMoS-Results/BestModel/BestModel",mrun,".rds"))

      }else{# if method did not return a better result
        switch(method, "forward" = {#if forward failed
          if(previous == "forward"){
            method <- "backward"
          }else if(previous == "backward"){
            if(no.crit == FALSE || no.swap == FALSE){
              method <- "swap"
            }else{
              cat("Best model found. Algorithm stopped.", sep = "\n")
              final.results <- return.results(homedir, mrun, ic = information.criterion)

              timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
              cat(paste0("Time needed: ",
                         sprintf("%02d:%02d:%02d",
                                 timediff %% 86400 %/% 3600,  # hours
                                 timediff %% 3600 %/% 60,  # minutes
                                 timediff %% 60 %/% 1), # seconds,
                         sep = "\n"))

              #setwd(old.directory)
              return(final.results)
            }
          }else if(previous == "swap"){
            method <- "backward"
          }
          previous <- "forward"

        }, "backward" = {#if backward failed
          if(previous == "forward"){
            if(no.crit == FALSE || no.swap == FALSE){
              method <- "swap"
            }else{
              cat("Best model found. Algorithm stopped.", sep = "\n")

              final.results <- return.results(homedir, mrun, ic = information.criterion)
              timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
              cat(paste0("Time needed: ",
                         sprintf("%02d:%02d:%02d",
                                 timediff %% 86400 %/% 3600,  # hours
                                 timediff %% 3600 %/% 60,  # minutes
                                 timediff %% 60 %/% 1), # seconds,
                         sep = "\n"))
              #setwd(old.directory)
              return(final.results)
            }

          }else if(previous == "backward"){
            method <- "forward"
          }
          previous <- "backward"

        }, "swap" = {#if swap failed
          # algorithm ends once swap method fails
          cat("Best model found. Algorithm stopped.", sep = "\n")
          final.results <- return.results(homedir, mrun, ic = information.criterion)
          timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
          cat(paste0("Time needed: ",
                     sprintf("%02d:%02d:%02d",
                             timediff %% 86400 %/% 3600,  # hours
                             timediff %% 3600 %/% 60,  # minutes
                             timediff %% 60 %/% 1), # seconds,
                     sep = "\n"))
          #setwd(old.directory)
          return(final.results)

        }
        )
        cat(paste0("Switch to method '", method, "'"), sep = "\n")
      }
    }

    if(save.performance == T){
      #save FAMoS performance
      if(ncol(saveTestedModels) > 3){
        famos.performance(input = saveTestedModels,
                          path = homedir,
                          ic = information.criterion,
                          save.output = paste0(homedir,"/FAMoS-Results/Figures/Performance",mrun,".pdf"))

        famos.performance(input = saveTestedModels,
                          path = homedir,
                          ic = information.criterion)
      }
    }

    #update model.run
    model.run <- model.run + 1
    timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
    cat(paste0("Time passed since start: ",
               sprintf("%02d:%02d:%02d",
                       timediff %% 86400 %/% 3600,  # hours
                       timediff %% 3600 %/% 60,  # minutes
                       timediff %% 60 %/% 1), # seconds,
               sep = "\n"))

  }

  cat("Computation completed.", sep = "\n")
}
