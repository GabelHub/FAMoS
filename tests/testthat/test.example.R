context("Test basic example")
library(future.batchtools)
#plan(sequential)
plan(multiprocess, workers = availableCores() - 1)
##system("rm -r tests/testthat/FAMoS-Results")
system("rm -r FAMoS-Results")

inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 1)

#create data with standard deviation of 1
x.values <- 1:7
y.values <-  9 * x.values^2 - exp(2 * x.values)
sd.y.values <- rep(1,7)

#define initial parameter values
inits <- c(p1 = 3, p2 = 4, p3 = -2, p4 = 2, p5 = 0)

#define cost function that returns the negative log-likelihood
cost_function <- function(parms, x.vals, y.vals, sd.y){
  # restrict the search range to -5 to +5
  if(max(abs(parms)) > 5){
    return(NA)
  }
  with(as.list(c(parms)), {
    res <- p1*4 + p2*x.vals + p3^2*x.vals^2 + p4*sin(x.vals)  - exp(p5*x.vals)
    diff <- sum((res - y.vals)^2/sd.y)
  })
}

swaps <- list(c("p1", "p2"), c("p4", "p5"))

ran.bord <- rep(0.1, length(inits))
ran.bord <- cbind(rep(-5, length(inits)), rep(5, length(inits)))

dont.fit <- c("p1")
#perform model selection
res <- famos(init.par = inits,
             fit.fn = cost_function,
             nr.of.data = length(y.values),
             homedir = getwd(),
             refit = TRUE,
             init.model.type = c("p1", "p3"),
             optim.runs = 1,
             future.off = FALSE,
             parscale.pars = FALSE,
             random.borders = ran.bord,
             #do.not.fit = dont.fit,
             x.vals = x.values,
             y.vals = y.values,
             sd.y = sd.y.values)

system("rm -r tests/testthat/FAMoS-Results")
system("rm -r FAMoS-Results")

test_that("Gives the correct output", {
  expect_match(res$binary, "00101")

})

