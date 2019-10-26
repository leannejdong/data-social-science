# List of packages for session
# The copula package requires gsl `sudo apt-get install libgsl-dev` over the terminal
# Nevertheless, I was able to compile over both of my machine hence I replaced the html here.
.pks<- c("tidyverse","magrittr","foreign","MASS","gamlss", "caret",  "gcmr")
# Install CRAN packages (if not already installed)
.inst<- .pks %in% installed.packages()
if(length(.pks[!.inst])>0) install.packages(.pks[!.inst])
# load packages into session
lapply(.pks,require,character.only=TRUE)

foo <- sa.inner %>%
  filter(Region == "Adelaide - South" | Region == "Adelaide - West")

myopt <- function (start, loglik, lower, upper) 
{
  require(nloptr)
  fn.opt <- function(x) {
    if (any(x <= lower || x >= upper)) 
      NA
    else -sum(loglik(x))
  }
  # ans <- optim(par = start, fn = fn.opt, method = method, control = control)
  start[is.na(start)] <- 0
  ans <- nloptr::lbfgs(x0 = start, fn = fn.opt,
                       lower = lower, upper = upper,
                       control = list(xtol_rel = 1e-8, maxeval = 10000, check_derivatives = F), 
                       nl.info = F)
  if (ans$convergence) 
    warning(paste("optim exits with code", ans$convergence))
  list(estimate = ans$par, maximum = ans$value, convergence = ans$convergence)
}
#break;
fm<-gcmr(x2 ~ mth.no + 
           # cos(mth.no * 2 * pi/12) + sin(mth.no * 2 * pi/12) + 
           as.factor(Region == "Adelaide - South") + 
           as.factor(Region == "Adelaide - West") +
           # as.factor(Region == "Adelaide - West") : as.factor(Region == "Adelaide - South") +
           as.factor(Region == "Barossa - Yorke - Mid North") +
           as.factor(Region == "South Australia - Outback") +
           as.factor(Region == "South Australia - South East") +
           as.factor(Month == 11),
         data = sa.inner, marginal=negbin.marg, cormat = arma.cormat(1,0), 
         options = list(seed = round(runif(1, 1, 1e+05)), nrep = c(100, 1000), no.se = FALSE, opt = myopt)) #nrep = 1000, 
fm$convergence <- 0
summary(fm)



fm<-gcmr(x2 ~ (mth.no + cos(mth.no * 2 * pi/12) + sin(mth.no *  2 * pi/12) ) * 
           (as.factor(Region=="Adelaide - North")+as.factor(Region=="Adelaide - South")+
              as.factor(Region=="Adelaide - West")+as.factor(Region=="Barossa - Yorke - Mid North")+
              as.factor(Region=="South Australia - Outback") + as.factor(Region=="South Australia - South East")) 
         + as.factor(Region)+ as.factor(Month == 11), 
         data = sa.inner, marginal=negbin.marg, cormat = arma.cormat(1,0), 
         options = list(seed = round(runif(1, 1, 1e+05)), nrep = c(100, 1000), no.se = FALSE, opt = myopt)) #nrep = 1000,
fm$convergence <- 0
summary(fm)


