
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FAMoS

FAMoS provides an automated and unbiased model selection algorithm that
aims at determining the most appropriate subset of model parameters to
describe a specific data set. Due to its flexibility with respect to the
cost/optimisation function, FAMoS can handle many different mathematical
structures, including for example regression models and ODEs.

## Installation

You can install FAMoS from github with:

``` r
# install.packages("devtools")
devtools::install_github("GabelHub/FAMoS")
# alternative installation command
devtools::install_git("git://github.com/GabelHub/FAMoS.git", branch = "master")
```

## Features

### Adaptive methods of model selection

FAMoS uses three different methods to find appropriate models to test:

- Forward search: Add a parameter to the currently best model
- Backward elimination: Remove a parameter from the currently best model
- Swap search: Replace a parameter of the currently best model by
  another. The parameters that can be swapped need to be specified by
  the user

FAMoS keeps track of the methods used in the previous iterations and
dynamically changes them according to the outcome of each iteration.

### Flexibility

FAMoS is designed to allow for a maximum of flexibility regarding the
fitting procedures and model types in R. While it comes with the default
option to fit the cost function via *optim*, it also allows the users to
specify their own optimisation routines, hence making it possible to
perform model selection based on various other R packages.

### Easy parallelisation

FAMoS makes use of the future-package which allows for easy
parallelisation, meaning many different models can be tested
simultaneously if the required computational resources are available.

### Smart testing procedures

FAMoS keeps track of previously tested models and checks also that each
model fulfills all user-specified restrictions, therefore testing only
relevant models and saving computational resources.

## Example

As a simple example, we generate a simple data set generated by two
parameters and apply FAMoS on a global model consisting of five
different parameters.

``` r
library(FAMoS)

#setting data
true.p2 <- 3
true.p5 <- 2
sim.data <- cbind.data.frame(range = 1:10, 
                             y = true.p2^2 * (1:10)^2 - exp(true.p5 * (1:10)))

#define initial parameter values and corresponding test function
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)

cost_function <- function(parms, binary, data){
  if(max(abs(parms)) > 5){
    return(NA)
  }
  with(as.list(c(parms)), {
    res <- p1*4 + p2^2*data$range^2 + p3*sin(data$range) + p4*data$range - exp(p5*data$range)
    diff <- sum((res - data$y)^2)
    
    #calculate AICC
    nr.par <- length(which(binary == 1))
    nr.data <- nrow(data)
    AICC <- diff + 2*nr.par + 2*nr.par*(nr.par + 1)/(nr.data - nr.par -1)
    
    return(AICC)
  })
}


#set swap set
swaps <- list(c("p1", "p5"))

#perform model selection
res <- famos(init.par = inits,
             fit.fn = cost_function,
             homedir = tempdir(),
             method = "swap",
             swap.parameters = swaps,
             init.model.type = c("p1", "p3"),
             optim.runs = 1,
             data = sim.data) 
```

FAMoS returns a lot of verbose output, telling the user what’s currently
happening (Note: The output can be turned on and off by using the option
*verbose*). In the beginning, the overall settings are defined and the
corresponding directories are created (if they don’t exist).

``` r
#> Initializing...
#> Create FAMoS directory...
#> 
#> Algorithm run: 001
#> Refitting disabled.
#> Starting algorithm with method 'swap'
```

In each iteration, FAMoS identifies new models to be tested based on the
current search method:

``` r
#> FAMoS iteration #3 - method: forward
#> Add parameter p1
#> Add parameter p2
#> Add parameter p4
#> Time passed since start: 00:00:00
```

Each model will be submitted and tested. Since FAMoS uses futures for
evaluation, the search process can be easily parallelised by setting the
corresponding future plan. Every model is subsequently evaluated by
performing (multiple) optimisation routines based either on the default
fitting routine *optim* or a user-specified fitting routine (see the
vignettes for examples).

After all models have been evaluated, the algorithm reads in the results
and checks, if a better model was found

``` r
#> Evaluate results ...
#> Best selection criterion value of this run is 10
#> Parameter p2 was added
#> Time passed since start: 1.92 secs
```

The cycle continues until no better model is found based on the
currently used methods. After halting, the results are returned

``` r
#> Best model found. Algorithm stopped.
#> FAMoS run 001
#> Selection criterion value of best model: 7
#> Best model (binary): 01001
#> Best model (vector):
#> p1 p2 p3 p4 p5 
#>  0  1  0  0  1 
#> Estimated parameter values:
#> p1 p2 p3 p4 p5 
#>  0  -3 0  0  2 
#> Time needed: 3.84 secs
```

## FAMoS options in detail

#### init.par

The vector *init.par* is one of two mandatory variables that need to be
specified. It contains the names and initial values of *all* model
parameters, that FAMoS is supposed to analyse. In our example above, we
specified this vector as

``` r
#define initial parameter values
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
```

Depending on the starting model, FAMoS automatically extracts the
corresponding values and uses them for its first iteration only. All
following iterations inherit the best values from previous fits.

Additional specifications for the use of the inital parameter vector can
be supplied by the options *do.not.fit* and *default.val*.

#### fit.fn

To allow independence of specific mathematical model structures, the
user can specify any cost or optimisation function. If a cost function
is used, it has to take the complete parameter vector as an input (names
*parms*) and has to return a selection criterion value. If use.optim =
TRUE, the cost function needs to return a single numeric value, which
corresponds to the selection criterion value. However, if use.optim =
FALSE, the cost function needs to return a list containing in its first
entry the selection criterion value and in its second entry the named
vector of the fitted parameter values (non-fitted parameters are
internally assessed).

Additionally, the cost and optimisation functions can also use the
optional input *binary*, which contains the binary information of the
current model, i.e. the information which parameters are currently
considered to be fitted. This is useful to extract the to-be-fitted
parameters, if a custom optimisation functions is used

Due to this flexible structure, FAMoS is able to tackle many different
problems, e.g. modelling approaches like linear regression, ODEs or
PDEs.

#### homedir

FAMoS generates and saves many different files, in order to make results
available over time as well as to simultaneously running FAMoS runs.
*homedir* specifies the folder, in which all results are going to be
stored. The default is set to the current working directory.

#### do.not.fit

In order to exclude some parameters from the fitting procedures, their
names can be specified in the *do.not.fit* option. This allows to test
different model restrictions without needing to change either *init.par*
or *fit.fn*. For example, if we wanted to exclude the parameter *p4*
from our analysis, we would specify initially

``` r
#define initial parameter values
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
no.fit <- c("p4")
```

and pass this option on to FAMoS. Note that excluded parameters are
automatically removed from the initial model, if *init.model.type =
“random”*, *init.model.type = “global”* or *init.model.type =
“most.distant”* is used. If the user-specified initial model contains an
excluded parameter, an error will be returned.

``` r
The specified initial model violates critical conditions or the do.not.fit specifications
```

#### method

FAMoS can use three different methods to search for different models to
test: Forward search, backward elimination and swap search. As the
algorithm dynamically changes these methods over the course of each
iteration, the option *method* only specifies the starting method.

If the algorithm is able to find a better model, the current method will
be used in the next iteration as well (except the swap method, which
always uses a forward search next - if it doesn’t terminate in that
step). If no better model is found, the algorithm will change the method
according to the following scheme:

| current method |   previous method   |     next method     |
|:--------------:|:-------------------:|:-------------------:|
|    forward     |      backward       | swap (or terminate) |
|    forward     |   forward or swap   |      backward       |
|    backward    |      backward       |       forward       |
|    backward    |       forward       | swap (or terminate) |
|      swap      | forward or backward |      terminate      |

In case the swap method is not used (due to unspecified critical or swap
sets), the algorithm will terminate after a succession of an
unsuccessful forward and backward search.

#### init.model.type

To verify if FAMoS results are consistent, it is important to run the
algorithm with different starting models. To set the initial model, the
user can either use the built-in options *random* (which generates a
random model), *global* (which uses the complete model as a starting
point) or *most.distant* (uses the model most dissimilar to all
previously tested models). Alternatively, the user can specify a model
by supplying a parameter vector containing the names of the initial
model.

``` r
#Three options for the starting model
init.model1 <- "random" # generates a random starting model
init.model2 <- "global" # uses all available parameters
init.model3 <- "most.distant" # uses the most dissimilar model
init.model4 <- c("p1", "p4") # a user-specified model
```

In case *random*, *global* or *most.distant* are chosen, FAMoS
automatically applies critical conditions and removes excluded
parameters (see options *critical.parameters* and *do.not.fit*).

#### refit

Before testing a model, FAMoS checks if this model has been tested
before. In case *refit = FALSE* (default) is specified, the model will
not be tested again. If refitting is set to TRUE, FAMoS will try to
optimise the model again. If the new run returns a better fit, the old
results will be overwritten, otherwise the new run will be discarded.

Refitting makes sense if the model optimisation is dependent on the
initial parameter combination (see also *optim.runs*). If a model is
reencountered, it might well be that the new parameter set to be tested
with is much more appropriate than the previous one, especially if this
reencounter happens within the same FAMoS run.

#### use.optim

The default fitting routine that FAMoS relies on is the built-in
function *optim*. However, by setting *use.optim = FALSE*, the user can
use any other optimisation routine suitable. The optimisation routine
then has to be included in the cost function *fit.fn* which needs to
return a list containing the current selection value criterion as well
as the parameter values used. See the vignettes for an example.

#### optim.runs

Finding the best fit for each model is crucial to guarantee a correct
model selection procedure. Often, fitting a model once is enough and
repeating the fitting procedure with different initial conditions does
not lead to new results. Sometimes, however, one wants to run multiple
fits for each model, e.g. if the parameter space is very large. To do
so, the user can specify *optim.runs*, which gives the number of fitting
attempts. For each optimisation run a *different* starting condition is
used. The first fitting attempt takes the inherited parameter vectors
from previous runs, while all following fitting attempts randomly
samples parameter vectors to test (see also *random.borders*).

If multiple optimisation runs are performed, FAMoS will return the best
of these runs.

In each optimisation run fitting in FAMoS is either performed with the
built-in function *optim*, which is repeatedly evaluated until
convergence, or a custom optimisation routine, which is evaluated only
once. As the default optimisation method is based on the Nelder-Mead
approach, which often tends to not give reliable results if only one
optimisation is performed, the optimisation for each fitting attempt is
wrapped into a while-loop, in which the fitting procedure is repeatedly
halted and restarted (based on the options *control.optim*), until the
relative convergence tolerance in *con.tol* is reached.

The skeleton of the underlying code looks like this:

``` r

for(i in 1:optim.runs){#number of fitting attempts specified by optim.runs
  start.parameters <- either the inherited or a randomly sampled set (for i > 2)
  
  if(use.optim == TRUE){
    #If use.optim = TRUE, the fitting routine is evaluated in a while loop
    while(abs((old.optim.value - new.optim.value)/old.optim.value) < con.tol){
      ... run optim with start.parameters ...
      start.parameters <- new parameters estimated by optim
    }
  }else{
    #If use.optim = FALSE, the custom optimisation routine is evaluated 
    #only once in each optimisation run
    ... run custom optimisation with start parameters ...
  }
  
}
```

#### default.val

Normally, FAMoS sets the parameters that are not fitted equal to zero.
However, this might not be appropriate if, for example, a parameter
describes an initial condition or a baseline turnover. Here,
*default.val* allows to specify the value that a parameter assumes, if
it is not fitted. *default.val* needs to be given as a named list, which
can either store numerical values or the name of the parameter from
which the value should be inherited. For example

``` r
#define initial parameter values
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)
#set default values
def.val <- list(p1 = 2, p2 = -5, p3 = "p1", p4 = 0, p5 = "p4")
```

Here, the values of *p1*, *p2*, and *p4* are set to their respective
values. However, *p3* and *p5* will inherit their values from *p1* and
*p4*, respectively. This feature is useful if two rates describe similar
processes and one wants to test if the difference between them is
significant enough to warrant the fitting of an additional parameter.
Here’s a short example

``` r
cost.function <- function(parms){
  x <- par1 + par2*x
  y <- par3 + par4*x
}

def.val <- list(p1 = 0, p2 = 0, p3 = "p1", p4 = "p2")
```

Note that the parameter inheritance cannot be chained, meaning that
entries that point to another parameter need a numeric value to access

``` r
#INCORRECT use of default.val
def.val <- list(p1 = 1, p2 = "p1", p3 = "p2", p4 = "p3")
#CORRECT use of default.val
def.val <- list(p1 = 1, p2 = "p1", p3 = "p1", p4 = "p1")
```

#### swap.parameters

The swap search that FAMoS can perform relies on sets which specify
parameters that can be swapped by one another. For example, if we wanted
to allow parameters *p1*, *p2* and *p3*, as well as *p4* and *p5* to be
replaceable by each other, we would specify:

``` r

swap.set <- list(c("p1", "p2", "p3"), c("p4", "p5"))
```

#### critical.parameters

In some cases, it does not make sense to fit certain submodels of the
global model to the data, as they might lack crucial parameters. FAMoS
can incorporate these restrictions by the specification of critical
parameter sets. For example, if at least one of the first three
parameters need to be present in the model, and all models that don’t
feature *p4* are not correct, we can specify:

``` r
crit.set <- list(c("p1", "p2", "p3"), c("p4"))
```

All critical sets are also automatically used in the swap search.

#### random.borders

Since the parameters of all *optim.runs* larger than one are sampled
based on a random uniform distribution, it might be important to set the
correct sampling intervals. By default, FAMoS samples parameters with a
100% deviation of the inherited parameter values (for example, if a
model contains two parameters, and the currently best values are *p1 =
0.1* and *p2 = -1000*, the sampled values will lie in the intervals
\[0,0.2\] and \[-2000,0\], respectively). Alternatively, the user can
specify relative or absolute sampling intervals. For relative intervals,
a numeric value has to be given for each parameter denoting its relative
deviation. For absolute sampling intervals, a matrix containing the
lower and upper borders has to be specified. Here’s an example:

``` r
#relative sampling ranges
random.bord1 <- 0.3 # deviates all parameters by 30%
random.bord2 <- c(0.1, 0.5, 0.2) # deviates the parameters by 10%, 50% and 20%, respectively

#absolute sampling ranges
random.bord3 <- matrix(c(1,2), nrow = 1) #uses the interval [1,2] for all parameter samples
random.bord4 <- cbind(c(0,-10, 0.3), c(5, -9, 0.7)) #uses the intervals [0,5], [-10,-9] and [0.3, 0.7] to sample the respective parameters

#use a function to sample the results
random.bord5 <- rnorm #note that in this case, 'mean' and 'sd' need to passed to famos as well, if other values than the default settings should be used
```

#### control.optim

Specifies the control options used for *optim* (see *optim.runs* for
more details).

#### parscale.pars

If parameters values span over several orders of magnitudes, using the
built-in option *parscale* in *optim* can reduce the numbers of
evaluations needed. Setting *parscale.pars = TRUE* automatically adjusts
the scaling procedure in *optim*. In our experience, using
*parscale.pars = TRUE* is usually beneficial if a large number of
parameters with different orders of magnitude need to be fitted.
However, the actual performance is very problem-specific and therefore
we would recommend initially testing both approaches to see which one
performs better for the problem at hand. Also, one needs to make sure
that the other options given in *control.optim* and *con.tol* are
specified appropriately.

#### con.tol

Specifies the relative convergence tolerance and determines when the
repeated use *optim* fits will be terminated (see *optim.runs* for more
details).

#### save.performance

If true, a plot of the current FAMoS performance is stored in the folder
“FAMoS-Results/Figures/”, which will be updated during each iteration.

#### use.futures

To allow for parallelisation, FAMoS uses the *future* package by Henrik
Bengtsson (see <https://github.com/futureverse/future>). To use futures,
the option needs to be set to TRUE and a future plan needs to be
specified.

#### reattempt

By default, FAMoS terminates once all search methods are exhausted.
However, if *reattempts* is set to true, FAMoS will instead jump to a
distant model and continues to search the model space from there. The
algorithm is then terminated if the best model is re-encountered (or if
no other models are available to test).

#### log.interval

If futures are used during a FAMoS run, there will be a message printed
every *X* seconds, informing the user which models fits are still
running. *log.interval* allows to specify the interval of *X*. Default
to 10 minutes (600 seconds).

#### interactive.session

As FAMoS allows to use previously generated results in later runs, it
performs a consistency check to see if the previous results were
generated by the same cost function. If this is not the case, FAMoS
requires user input to decide what to do next. However, if run
non-locally, supplying user input might not b possible. Therefore,
*interactive.session* can be set to FALSE. This will result in FAMoS
issuing a warning instead of an interaction prompt.

#### verbose

The verbose output of FAMoS can be turned on and off. If *verbose =
FALSE*, only a minimum of information is shown.
