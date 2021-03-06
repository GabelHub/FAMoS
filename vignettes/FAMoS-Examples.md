
# Model selection on a system of ordinary differential equations

FAMoS was originally designed to perform model selection on systems of
ODEs, which is why this tool is especially suited to tackle these kind
of problems. Assume we have three different cell populations, *A*, *B*
and *C*. All of these cells types can divide and therefore increase in
their numbers. Furthermore, it is possible that all of these cells can
turn into each other. The underlying dynamics of these cell populations
can be described by a system of ordinary differential equations:

``` r
#define the global model dynamics
global.dynamics <- function(t,x,parms){
  with(as.list(c(x, parms)), {
    
    dA <- rho_A*A + mu_BA*B + mu_CA*C - (mu_AB + mu_AC)*C 
    dB <- rho_B*B + mu_AB*A + mu_CB*C - (mu_BA + mu_BC)*B 
    dC <- rho_C*C + mu_AC*A + mu_BC*B - (mu_CA + mu_CB)*C 
    
    return(list(c(dA,dB,dC)))
   }
  ) 
}
```

We will use this function to simulate some data using the *deSolve*
library:

``` r
#define simulation parameter set
pars <- c(rho_A = 0, rho_B = 0, rho_C = 0.1,
          mu_AB = 0.2, mu_AC = 0, mu_BA = 0, 
          mu_CA = 0, mu_BC = 0.05, mu_CB = 0)

#set initial values for cells
init.vals <- c(A = 100, B = 0, C = 0)

#simulate data
library(deSolve)

sim.data <- lsoda(y = init.vals, 
                  times = 0:10, 
                  func = global.dynamics, 
                  parms = pars)
```

Now, we will define the cost function for FAMoS:

``` r
#cost function for famos
cost.function <- function(parms, binary, data, inits){
  #simulate the data with the current parameter set
  fit.data <- lsoda(y = inits, 
                    times = 0:10, 
                    func = global.dynamics, 
                    parms = parms)
  #calculate the aic
  ls2 <- sum((sim.data[,-1] - fit.data[,-1])^2)
  out.aic <- ls2 + 2*sum(binary == 1)
  
  return(out.aic)
}
```

To call FAMoS, we need to specify a vector containing the names and the
values of the start parameters. All that’s left is calling the model
selection routine then. The fitting might take a couple of minutes. To
see more of the model selection process, set *verbose = TRUE*.

``` r

library(FAMoS)

start.vals <- c(rho_A = 0.1, rho_B = 0.1, rho_C = 0.1,
                mu_AB = 0.1, mu_AC = 0.1, mu_BA = 0.1, 
                mu_CA = 0.1, mu_BC = 0.1, mu_CB = 0.1)

famos.fit <- famos(init.par = start.vals,
                   fit.fn = cost.function,
                   homedir = tempdir(),
                   init.model.type = c("rho_A"),
                   data = sim.data,
                   inits = init.vals)

print(famos.fit)
```

# Model selection based on logistic regression models

There are many R packages available that tackle the problem of model
selection based on regression models and FAMoS is not meant to replace
them. However, it is also possible (and not even that difficult) to
apply FAMoS to these kind of problems. For our example, we will use the
birthwt data set from the *MASS* package, a standard test set for model
selection. We will follow the modifications by Venables and Ripley in
their book *Modern Applied Statistics with S-PLUS* and transform the
data set in the following way before using it:

``` r
library(MASS)

attach(birthwt)
race <- factor(race, labels = c("white","black","other"))
ptd <- factor(ptl > 0)
levels(ftv)[-c(1:2)] <- "2+"
bwt <- data.frame(low = factor(low), age, lwt, race,
                  smoke = (smoke>0), ptd, ht = (ht>0), ui =(ui>0), ftv)
detach();rm(race,ptd,ftv)
```

Now, we want to perform a model selection based on logistic regression
models with interactions. To this end, we will use the fitting routine
*glm* instead of the standard optimiser *optim*, which is the default
option in FAMoS. To tell FAMos, that we’re using a different optimiser,
we will set the option *use.optim = FALSE* and since we don’t need
multiple repeats with random starting values, we also set *optim.runs =
1*. Now, we need to supply and fitting function to FAMoS that - since we
are using another fitting routine - needs to return a list, that
contains the value of the selection criterion as well as the vector
containing the names and the values of the fitted variables. As we will
only work with the names in our example, having to return the parameter
values might seem a bit clunky. However, as FAMoS is intended to offer a
lot of flexibility, this feature is useful in many other cases (see
below). Our fitting function looks like this:

``` r
fit_func <- function(parms, data, binary){
  #First transform the parameter names into a formula. 
  #The to-be-fitted parameters are identified using the binary vector
  fitted.pars <- names(parms[which(binary == 1)])
  glm_formula <- as.formula(
    paste0("low ~ ", paste0(fitted.pars, collapse = "+"))
  )
  #fit the logistic model using glm
  out <- summary(
    glm(glm_formula, 
        family = binomial(link = logit),
        data = data
    )
  )
  
  #prepare output parameters, in this case only the names of the fitted 
  #parameters are relevant. As FAMoS also expects values with these, we will just 
  #return the value 1 for all parameters
  out.par <- rep(1, length(fitted.pars))
  names(out.par) <- fitted.pars
  
  #return a list containing the first the selection criterion value and second the 
  #vector of the fitted parameters
  return(list(SC = out$aic, params = out.par))
}
```

The only thing left is to specify the initial parameter vector. As we
also want to have interactions, we need to create entries for all
parameter combinations. Since *glm* expects them to be in the form of
*par1:par2*, we will name the entries
accordingly.

``` r
#first we define the available parameters. As the values are not important, we
#will just set them equal to 1.
inits <- c(age = 1, lwt = 1, race = 1, smoke = 1, ptd = 1, ht = 1, ui = 1, ftv = 1)
#Now, we calculate all possible interactions and name the corresponding vector
#entries accordingly
combinations <- combn(names(inits),2)
for(i in 1:ncol(combinations)){
  inits <- c(inits,1)
  names(inits)[i + 8] <- paste0(combinations[1,i],":",combinations[2,i])
}
```

All that’s left is calling FAMoS now:

``` r
library(FAMoS)
famos.glm <- famos(init.par = inits,
                   fit.fn = fit_func,
                   init.model.type = names(inits[1:8]),
                   data = bwt,
                   use.optim = FALSE,
                   optim.runs = 1)
```
