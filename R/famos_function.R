#' Automated Model Selection
#'
#' Given a vector containing all parameters of interest and a cost function, the \code{FAMoS} looks for the most appropriate subset model to describe the given data.
#' @param init.par A named vector containing the initial parameter values.
#' @param fit.fn A cost function. Has to take the complete parameter vector as an input (needs to be names \code{parms}) and must return a selection criterion value (e.g. AICc or BIC). See Details for more information.
#' @param homedir The directory to which the results should be saved to.
#' @param do.not.fit The names of the parameters that are not supposed to be fitted. Default is NULL.
#' @param method The starting method of the FAMoS. Options are "forward" (forward search), "backward" (backward elimination) and "swap" (only if \code{critical.parameters} or \code{swap.parameters} are supplied). Methods are adaptively changed over each iteration of the FAMoS. Default to "forward".
#' @param init.model.type The starting model. Options are "global" (starts with the complete model), "random" (creates a randomly sampled starting model) or "most.distant" (uses the model most dissimilar from all other previously tested models). Alternatively, a specific model can be used by giving the corresponding names of the parameters one wants to start with. Default to "random".
#' @param refit If TRUE, previously tested models will be tested again. Default to FALSE.
#' @param use.optim Logical. If true, the cost function \code{fit.fn} will be fitted via \code{\link{optim}}. If FALSE, the cost function will only be evaluated.
#' @param optim.runs The number of times that each model will be optimised. Default to 1. Numbers larger than 1 use random initial conditions (see \code{random.borders}).
#' @param default.val A named list containing the values that the non-fitted parameters should take. If NULL, all non-fitted parameters will be set to zero. Default values can be either given by a numeric value or by the name of the corresponding parameter the value should be inherited from (NOTE: In this case the corresponding parameter entry has to contain a numeric value). Default to NULL.
#' @param swap.parameters A list specifying which parameters are interchangeable. Each swap set is given as a vector containing the names of the respective parameters. Default to NULL.
#' @param critical.parameters A list specifying sets of critical parameters. Critical sets are parameters sets, of which at least one parameter per set has to be present in each tested model. Default to NULL.
#' @param random.borders The ranges from which the random initial parameter conditions for all \code{optim.runs} larger than one are sampled. Can be either given as a vector containing the relative deviations for all parameters or as a matrix containing in its first column the lower and in its second column the upper border values. Parameters are uniformly sampled based on \code{\link{runif}}. Default to 1 (100\% deviation of all parameters). Alternatively, functions such as \code{\link{rnorm}}, \code{\link{rchisq}}, etc. can be used if the additional arguments are passed along as well.
#' @param control.optim Control parameters passed along to \code{optim}. For more details, see \code{\link{optim}}.
#' @param parscale.pars Logical. If TRUE, the \code{parscale} option will be used when fitting with \code{\link{optim}}. This can help to speed up the fitting procedure, if the parameter values are on different scales. Default to FALSE.
#' @param con.tol The absolute convergence tolerance of each fitting run (see Details). Default is set to 0.1.
#' @param save.performance Logical. If TRUE, the performance of \code{FAMoS} will be evaluated in each iteration via \code{\link{famos.performance}}, which will save the corresponding plots into the folder "FAMoS-Results/Figures/" (starting from iteration 3) and simultaneously show it on screen. Default to TRUE.
#' @param future.off Logical. If TRUE (default), FAMoS runs without the use of \code{futures}.
#' @param log.interval The interval (in seconds) at which FAMoS informs about the current status, i.e. which models are still running and how much time has passed. Default to 600 (= 10 minutes).
#' @param verbose Logical. If TRUE, FAMoS will output all details about the current fitting procedure.
#' @param ... Other arguments that will be passed along to \code{\link{future}}, \code{\link{optim}} or the user-specified cost function \code{fit.fn}.
#' @details In each iteration, the FAMoS finds all neighbouring models based on the current model and method, and subsequently tests them. If one of the tested models performs better than the current model, the model, but not the method, will be updated. Otherwise, the method, but not the model, will be adaptively changed, depending on the previously used methods.
#' 
#' The cost function \code{fit.fn} can take the following inputs:
#' \describe{
#'   \item{parms}{A named vector containing all parameter values. This input is mandatory. If \code{use.optim = TRUE}, FAMos will automatically subset the complete parameter set into fitted and non-fitted parameters.} 
#'   \item{binary}{Optional input. The binary vector contains the information which parameters are currently fitted. Fitted parameters are set to 1, non-fitted to 0. This input can be to split the complete parameter set into fitted and non-fitted parameters if a customised optimisation function is used (see \code{use.optim}).}
#'   \item{...}{Other parameters that can should be passed to \code{fit.fn}}
#'   }
#'
#'If \code{use.optim = TRUE}, the cost function needs to return a single numeric value, which corresponds to the selection criterion value. However, if \code{use.optim = FALSE}, the cost function needs to return a list containing in its first entry the selection criterion value and in its second entry the named vector of the fitted parameter values (non-fitted parameters are internally assessed).   
#'    
#' @export
#' @return A list containing the following elements:
#' \describe{
#'   \item{SCV}{The value of the selection criterion of the best model.}
#'   \item{par}{The values of the fitted parameter vector corresponding to the best model.}
#'   \item{binary}{The binary information of the best model.}
#'   \item{vector}{Vector indicating which parameters were fitted in the best model.}
#'   \item{total.models.tested}{The total number of different models that were analysed. May include repeats.}
#'   \item{mrun}{The number of the current FAMoS run.}
#'   \item{initial.model}{The first model evaluated by the FAMoS run.}
#'   }
#' @examples
#'
#' #setting data
#' true.p2 <- 3
#' true.p5 <- 2
#' sim.data <- cbind.data.frame(range = 1:10, 
#'                              y = true.p2^2 * (1:10)^2 - exp(true.p5 * (1:10)))
#' 
#' #define initial parameter values and corresponding test function
#' inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
#' 
#' cost_function <- function(parms, binary, data){
#'   if(max(abs(parms)) > 5){
#'     return(NA)
#'   }
#'   with(as.list(c(parms)), {
#'     res <- p1*4 + p2^2*data$range^2 + p3*sin(data$range) + p4*data$range - exp(p5*data$range)
#'     diff <- sum((res - data$y)^2)
#'     
#'     #calculate AICC
#'     nr.par <- length(which(binary == 1))
#'     nr.data <- nrow(data)
#'     AICC <- diff + 2*nr.par + 2*nr.par*(nr.par + 1)/(nr.data - nr.par -1)
#'     
#'     return(AICC)
#'   })
#' }
#' 
#'
#' #set swap set
#' swaps <- list(c("p1", "p5"))
#'
#' #perform model selection
#' famos(init.par = inits,
#'       fit.fn = cost_function,
#'       homedir = tempdir(),
#'       method = "swap",
#'       swap.parameters = swaps,
#'       init.model.type = c("p1", "p3"),
#'       optim.runs = 1,
#'       data = sim.data)
#'       
#' #delete tempdir
#' unlink(tempdir(), recursive = TRUE)

famos <- function(init.par,
                  fit.fn,
                  homedir = getwd(),
                  do.not.fit = NULL,
                  method = "forward",
                  init.model.type = "random",
                  refit = FALSE,
                  use.optim = TRUE,
                  optim.runs = 5,
                  default.val = NULL,
                  swap.parameters = NULL,
                  critical.parameters = NULL,
                  random.borders = 1,
                  control.optim = list(maxit = 1000),
                  parscale.pars = FALSE,
                  con.tol = 0.1,
                  save.performance = TRUE,
                  future.off = TRUE,
                  log.interval = 600,
                  verbose = FALSE,
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
  
  #reset graphical parameters on exit
  old.par <- graphics::par("mfrow")
  on.exit(graphics::par(mfrow = old.par))
  
  cat("\nInitializing...", sep = "\n")
  
  #set starting time
  start <- Sys.time()
  #create FAMoS directory
  cat("Create FAMoS directory...", sep = "\n")
  make.directories(homedir)
  if(file.exists(paste0(homedir, "/FAMoS-Results/FAMoS-Info.rds"))){
    old.session <- readRDS(paste0(homedir, "/FAMoS-Results/FAMoS-Info.rds"))
    if(is.character(all.equal(fit.fn, old.session$fit.function))){
      user.input <- readline(cat("The previous results were generated by a different cost function and might therefore not be usable during this run. What should FAMoS do?\n 1) Continue anyway   2) Delete old results and continue   3) Halt\nEnter number here:"))
      switch (user.input,
        "1" = {},
        "2" = {unlink(paste0(homedir, "/FAMoS-Results"),recursive = TRUE)
               make.directories(homedir)
               famos.session <- list(fit.function = fit.fn)
               saveRDS(famos.session,paste0(homedir, "/FAMoS-Results/FAMoS-Info.rds"))},
        {stop("FAMos halted.")}
      )
     
    }
  }else{
    famos.session <- list(fit.function = fit.fn)
    saveRDS(famos.session,paste0(homedir, "/FAMoS-Results/FAMoS-Info.rds"))
  }
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
  
  if(length(init.model.type) == 1 && (init.model.type == "global" || init.model.type == "random" || init.model.type == "most.distant")){
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
            },
            "most.distant" = {
              #check if previous files exist
              filenames <- list.files(paste0(homedir,"/FAMoS-Results/TestedModels/"), 
                                      pattern="*.rds", 
                                      full.names=TRUE)
              if(length(filenames) == 0){
                stop("No previously tested models available. Please use another option for init.model.type.")
              }
              #get a random initial model
              init.model <- which(get.most.distant(input = homedir)[[3]] == 1)
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
  #create storage for SCVs for the tested models
  save.SCV <- c()
  
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
          if(verbose){
            cat(paste0("Add parameter ", all.names[parms.left[j]]), sep = "\n")
          }
          
          
          #get the indices of the currently tested parameter set
          pick.model <- c(parms.left[j], pick.model.prev)
          
          #get tag of current model (NOTE: I binarised the model ID. Model0110 means that parameter 1 was not included, parameter 2 was included, parameter 3 was included, parameter 4 was not included)
          curr.model <- rep(0,length(init.par))
          curr.model[pick.model] <- 1
          
          #test if model violates the critical conditions
          if(model.appr(pick.model, crit.parms, do.not.fit = do.not.fit) == FALSE){
            if(verbose){
              cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
            }
            
            next
          }
          #check if model has been tested before
          if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
            if(verbose){
              cat("Combination has been tested before", sep = "\n")
            }
            
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
          if(verbose){
            cat(paste0("Remove parameter ", all.names[parms.left[j]]), sep = "\n")
          }
          
          
          # pick.model <- c(sample(x = samp.vec[-pick.model.prev], size = 1), pick.model.prev)
          pick.model <- c(parms.left[-j])
          
          
          curr.model <- rep(0,length(init.par))
          curr.model[pick.model] <- 1
          
          #test if model violates the critical conditions
          if(model.appr(pick.model, crit.parms, do.not.fit = do.not.fit) == FALSE){
            if(verbose){
              cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
            }
           
            nosub <- c(nosub, j)
            next
          }
          #check if model has been tested before
          if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
            if(verbose){
              cat("Combination has been tested before", sep = "\n")
            }
            
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
              
              if(verbose){
                cat(paste0("Replace ", all.names[cmb[j,1]], " by ", all.names[cmb[j,2]], "\n"))
              }
              
              #check if model has been tested before while refitting is not enabled
              if(is.element(0,colSums(abs(models.tested-curr.model))) && refit==FALSE){
                if(verbose){
                  cat("Combination has been tested before", sep = "\n")
                }
                
                next
              }
              
              #test if model violates the critical conditions
              pick.model <- which(curr.model != 0)
              if(model.appr(pick.model, crit.parms,do.not.fit = do.not.fit) == FALSE){
                if(verbose){
                  cat(paste("Model ", paste0(curr.model, collapse=""), " violates critical parameter specifications. Model skipped."), sep = "\n")
                }
               
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
          final.results <- return.results(homedir, mrun)
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
      save.SCV <- cbind(save.SCV, NA)
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
    
    if(verbose){
      cat("Job submission:", sep = "\n")
    }
    
    #Job submission####
    #submit each individual model to the cluster if it hasn't been tested before
    for(j in 1:ncol(curr.model.all)){
      if(file.exists(paste0(homedir, "/FAMoS-Results/Fits/Model",paste(curr.model.all[,j], collapse =""), ".rds")) == FALSE  ||  refit){
        
        if(verbose){
          cat(paste0("Job ID for model ", formatC(j, width = 2, format = "d", flag = "0"), " - ", paste(curr.model.all[,j], collapse="")), sep = "\n")
        }
        
        
        if(future.off == F){
          assign(paste0("model",j),
                 future::future({
                   base.optim(binary = curr.model.all[,j],
                              parms = best.par,
                              fit.fn = fit.fn,
                              homedir = homedir,
                              use.optim = use.optim,
                              optim.runs = optim.runs,
                              default.val = default.val,
                              random.borders = random.borders,
                              control.optim = control.optim,
                              parscale.pars = parscale.pars,
                              scaling = scaling.values,
                              con.tol = con.tol,
                              verbose = verbose,
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
                     homedir = homedir,
                     use.optim = use.optim,
                     optim.runs = optim.runs,
                     default.val = default.val,
                     random.borders = random.borders,
                     control.optim = control.optim,
                     parscale.pars = parscale.pars,
                     scaling = scaling.values,
                     con.tol = con.tol,
                     verbose = verbose,
                     ...)
        }
        
        
      }else{
        assign(paste0("model",j), "no.refit")
        if(verbose){
          cat(paste0("Model fit for ", paste(curr.model.all[,j], collapse="")," exists and refitting is not enabled."), sep = "\n")
        }
        
      }
    }
    
    if(future.off == FALSE){
      #set looping variable
      waiting <- TRUE
      waited.models <- rep(1,ncol(curr.model.all))
      time.waited <- Sys.time()
      time.passed <- -1
      ticker <- 0
      ticker.time <- log.interval
      
      #check if the job is still running. If the job is not running anymore restart.
      while(waiting == TRUE){
        update.log <- FALSE
        waiting <- FALSE
        #cycle through all models
        for(j in which(waited.models == 1)){
          
          if(future::resolved(get(paste0("model", j))) == FALSE){
            waiting <- TRUE
          }else{
            
            if(class(try(future::value(get(paste0("model", j)), std = FALSE))) == "try-error"){
              stop(paste0("Future failed. The corresponding error message of job ",
                          paste(curr.model.all[,j], collapse=""),
                          " is shown above. If no output is shown, use 'future.off = TRUE' to debug."))
            }
            
            #check if output was generated, including waiting period if the cluster is very busy
            if(!file.exists(paste0(homedir,
                                   "/FAMoS-Results/Fits/Model",
                                   paste(curr.model.all[,j], collapse=""),
                                   ".rds"))){
              Sys.sleep(10)
            }
            
            if(!file.exists(paste0(homedir,
                                   "/FAMoS-Results/Fits/Model",
                                   paste(curr.model.all[,j], collapse=""),
                                   ".rds"))){
              
              
              stop("Future is done but no output file to job ",
                   paste(curr.model.all[,j], collapse=""),
                   " was created. FAMoS halted.")
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
        
        time.passed <- round(difftime(Sys.time(),time.waited, units = "secs")[[1]],2) - ticker*ticker.time
        
        #output the log for the models that is waited for (every 5 min)
        if( (time.passed > ticker.time) ){
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
    get.SCV <- c()
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
      get.SCV <- cbind(get.SCV,
                        readRDS(paste0(homedir,
                                       "/FAMoS-Results/Fits/Model",
                                       paste(curr.model.all[,j], collapse =""),
                                       ".rds")))
    }
    
    #save the resulted SCVs
    
    SCV <- get.SCV[1,]
    save.SCV <- c(save.SCV, SCV)
    
    #save SCVs with the corresponding models
    saveTestedModels <- rbind(save.SCV, models.per.run)
    row.names(saveTestedModels) <- c("SCV", "iteration", all.names)
    colnames(saveTestedModels) <- 1:length(colnames(saveTestedModels))
    saveRDS(saveTestedModels, paste0(homedir, "/FAMoS-Results/TestedModels/TestedModels",mrun,".rds"))
    
    #if more than one model was tested, order them according to their performance
    if(ncol(get.SCV) > 1){
      index.SCV <- which.min(get.SCV[1,])
      get.SCV <- get.SCV[,order(get.SCV[1,])]
      get.SCV <- as.numeric(get.SCV[,1])
    } else {
      index.SCV <- 1
      get.SCV <- as.numeric(get.SCV[,1])
    }
    
    names(get.SCV) <- c("SCV", all.names)
    
    #print currently best fit
    curr.SCV <- get.SCV[1]
    cat(paste0("Best selection criterion value of this run is ",round(curr.SCV,2)), sep = "\n")
    
    #update if new model is better
    if(model.run == 1){
      old.SCV <- curr.SCV
      pick.model.prev <- which(curr.model.all[,index.SCV] == 1)
      best.par <- get.SCV[-1]
      
      bm.bin <- curr.model.all[,index.SCV]
      
      saveRDS(get.SCV, paste0(homedir,
                               "/FAMoS-Results/BestModel/BestModel",
                               mrun,
                               ".rds"))
      
    }else{# if it's not the first run, we know what the previous and current method are
      
      #save FAMoS performance
      if(ncol(saveTestedModels) > 3){
        
        if(save.performance == T){
          famos.performance(input = saveTestedModels,
                            path = homedir,
                            save.output = paste0(homedir,
                                                 "/FAMoS-Results/Figures/Performance",
                                                 mrun,
                                                 ".pdf"))
        }

        famos.performance(input = saveTestedModels,
                          path = homedir)
        
      }
      
      if((curr.SCV) < old.SCV ){#update the model if a better model is found
        old.SCV <- curr.SCV
        pick.model.prev <- which(curr.model.all[,index.SCV] == 1)
        best.par <- get.SCV[-1]
        
        bm.bin <- curr.model.all[,index.SCV]
        
        #update previous and current method and output everything to the log file
        switch(method, "forward" = {
          previous <- "forward"
          method <- "forward"
          cat(paste0("Parameter ", all.names[parms.left[index.SCV]], " was added"), sep = "\n")
        }, "backward" = {
          previous <- "backward"
          method <- "backward"
          cat(paste0("Parameter ", all.names[parms.left[index.SCV]], " was removed"), sep = "\n")
        }, "swap" = {
          previous <- "swap"
          method <- "forward"
          cat(paste0("Parameter ", all.names[parms.left[index.SCV,2]], " was exchanged for ", all.names[parms.left[index.SCV,1]]), sep = "\n")
          cat(paste0("Switch to method '", method, "'"), sep = "\n")
        }
        )
        
        #save best SCV
        saveRDS(get.SCV, paste0(homedir, "/FAMoS-Results/BestModel/BestModel",mrun,".rds"))
        
      }else{# if method did not return a better result
        switch(method, "forward" = {#if forward failed
          if(previous == "forward"){
            method <- "backward"
          }else if(previous == "backward"){
            if(no.crit == FALSE || no.swap == FALSE){
              method <- "swap"
            }else{
              cat("Best model found. Algorithm stopped.", sep = "\n")
              final.results <- return.results(homedir, mrun)
              
              timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
              cat(paste0("Time needed: ",
                         sprintf("%02d:%02d:%02d",
                                 timediff %% 86400 %/% 3600,  # hours
                                 timediff %% 3600 %/% 60,  # minutes
                                 timediff %% 60 %/% 1), # seconds,
                         sep = "\n"))
              
              graphics::par(mfrow = c(1,2))
              sc.order(input = saveTestedModels,
                       mrun = mrun)
              
              aicc.weights(input = saveTestedModels,
                           mrun = mrun,
                           reorder = TRUE)
              
              if(save.performance == T){
                sc.order(input = saveTestedModels,
                         mrun = mrun,
                         save.output = paste0(homedir,"/FAMoS-Results/Figures/ModelComparison",mrun,".pdf"))
                
                aicc.weights(input = saveTestedModels,
                             mrun = mrun,
                             reorder = TRUE,
                             save.output = paste0(homedir,"/FAMoS-Results/Figures/AkaikeWeights",mrun,".pdf"))
              }
              
              
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
              
              final.results <- return.results(homedir, mrun)
              timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
              cat(paste0("Time needed: ",
                         sprintf("%02d:%02d:%02d",
                                 timediff %% 86400 %/% 3600,  # hours
                                 timediff %% 3600 %/% 60,  # minutes
                                 timediff %% 60 %/% 1), # seconds,
                         sep = "\n"))
              
              graphics::par(mfrow = c(1,2))
              sc.order(input = saveTestedModels,
                       mrun = mrun)
              
              aicc.weights(input = saveTestedModels,
                           mrun = mrun,
                           reorder = TRUE)
              
              if(save.performance == T){
                sc.order(input = saveTestedModels,
                         mrun = mrun,
                         save.output = paste0(homedir,"/FAMoS-Results/Figures/ModelComparison",mrun,".pdf"))
                
                aicc.weights(input = saveTestedModels,
                             mrun = mrun,
                             reorder = TRUE,
                             save.output = paste0(homedir,"/FAMoS-Results/Figures/AkaikeWeights",mrun,".pdf"))
              }
              
              return(final.results)
            }
            
          }else if(previous == "backward"){
            method <- "forward"
          }
          previous <- "backward"
          
        }, "swap" = {#if swap failed
          # algorithm ends once swap method fails
          cat("Best model found. Algorithm stopped.", sep = "\n")
          final.results <- return.results(homedir, mrun)
          timediff <- difftime(Sys.time(),start, units = "secs")[[1]]
          cat(paste0("Time needed: ",
                     sprintf("%02d:%02d:%02d",
                             timediff %% 86400 %/% 3600,  # hours
                             timediff %% 3600 %/% 60,  # minutes
                             timediff %% 60 %/% 1), # seconds,
                     sep = "\n"))
          
          graphics::par(mfrow = c(1,2))
          sc.order(input = saveTestedModels,
                   mrun = mrun)
          
          aicc.weights(input = saveTestedModels,
                       mrun = mrun,
                       reorder = TRUE)
          
          if(save.performance == T){
            sc.order(input = saveTestedModels,
                     mrun = mrun,
                     save.output = paste0(homedir,"/FAMoS-Results/Figures/ModelComparison",mrun,".pdf"))
            
            aicc.weights(input = saveTestedModels,
                         mrun = mrun,
                         reorder = TRUE,
                         save.output = paste0(homedir,"/FAMoS-Results/Figures/AkaikeWeights",mrun,".pdf"))
          }
          
          return(final.results)
          
        }
        )
        cat(paste0("Switch to method '", method, "'"), sep = "\n")
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
  
}