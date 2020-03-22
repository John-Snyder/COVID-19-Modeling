####
####
####

library(deSolve)
library(RColorBrewer)

#https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak#Cases_Chronology_in_Mainland_China
# Infected <- c(45, 62, 121, 198, 291, 440, 571, 830, 1287, 1975, 2744, 4515)
# day <- 0:(length(Infected)-1)
# N <- 1400000000 #pop of china

Infected <- covid19_long %>% filter(Province.State=="New York") %>% as.data.frame %>% select(Confirmed) %>% unlist %>% as.numeric
Infected[21] <- (Infected[20]+Infected[22])/2
day <- 0:(length(Infected)-1)
N <- 8000000

###edit 1: use different boundary condiotion
###init <- c(S = N-1, I = 1, R = 0)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
plot(day, Infected)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ####edit 2; use equally scaled variables 
  with(par, { dS <- -beta * (S/N) * I
  dI <- beta * (S/N) * I - gamma * I
  dR <- gamma * I
  list(c(dS, dI, dR))
  })
}

SIR2 <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  ####
  #### use as change of variables variable
  #### const = (beta-gamma)
  #### delta = gamma/beta
  #### R0 = beta/gamma > 1 
  #### 
  #### beta-gamma = beta*(1-delta)
  #### beta-gamma = beta*(1-1/R0)
  #### gamma = beta/R0
  with(par, { 
    beta  <- const/(1-1/R0)  
    gamma <- const/(R0-1)  
    dS <- -(beta * (S/N)      ) * I 
    dI <-  (beta * (S/N)-gamma) * I 
    dR <-  (             gamma) * I
    list(c(dS, dI, dR))
  })
}

RSS.SIR2 <- function(parameters) {
  names(parameters) <- c("const", "R0")
  out <- ode(y = init, times = day, func = SIR2, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected - fit)^2)
  return(RSS)
}

### plotting different values R0

# use the ordinary exponential model to determine const = beta - gamma
const <- coef(mod)[2]




RSS.SIR <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((Infected - fit)^2)
  return(RSS)
}

lower = c(0, 0)
upper = c(1, 1)  ###adjust limit because different scale 1/N

### edit: get a good starting condition
mod <- nls(Infected ~ a*exp(b*day), 
           start = list(a = Infected[1],
                        b = .5),
           control = nls.control(maxiter = 500, tol = 1e-05, minFactor = 1e-10,
                                 printEval = FALSE, warnOnly = FALSE)
)

optimsstart <- c(2,1)*coef(mod)[2]

set.seed(12)
Opt <- optim(optimsstart, RSS.SIR,
             method = "L-BFGS-B",
             lower = lower, upper = upper,
             hessian = TRUE)
Opt

### estimated covariance matrix of coefficients
### note the large error, but also strong correlation (nearly 1)
## note scaling with estimate of sigma because we need to use Hessian of loglikelihood
sigest <- sqrt(Opt$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt$hessian) 

####
####  using alternative parameters
####  for this we use the function SIR2
####

optimsstart <- c(coef(mod)[2],5)
lower = c(0, 1)
upper = c(1, 10^3)  ### adjust limit because we use R0 now which should be >1

set.seed(12)
Opt2 <- optim(optimsstart, RSS.SIR2, method = "L-BFGS-B",lower=lower, upper=upper,
              hessian = TRUE, control = list(maxit = 1000, 
                                             parscale = c(10^-3,1)))
Opt2

# now the estimated variance of the 1st parameter is small
# the 2nd parameter is still with large variance
#
# thus we can predict beta - gamma very well
# this beta - gamma is the initial growth coefficient
# but the individual values of beta and gamma are not very well known
#
# also note that hessian is not at the MLE since we hit the lower boundary
#
sigest <- sqrt(Opt2$value/(length(Infected)-1))
solve(1/(2*sigest^2)*Opt2$hessian)

#### We can also estimated variance by
#### Monte Carlo estimation
##
## assuming data to be distributed as mean +/- q mean
## with q such that mean RSS = 52030
##
## 
##


### Two functions RSS to do the optimization in a nested way
RSS.SIRMC2 <- function(const,R0,TS) {
  parameters <- c(const=const, R0=R0)
  out <- ode(y = init, times = day, func = SIR2, parms = parameters)
  fit <- out[ , 3]
  RSS <- sum((TS - fit)^2)
  return(RSS)
}
RSS.SIRMC <- function(const,TS) {
  optimize(RSS.SIRMC2, lower=1,upper=10^5,const=const,TS)$objective
}

getOptim <- function(TS) {
  opt1 <- optimize(RSS.SIRMC,lower=0,upper=1,TS=TS)
  opt2 <- optimize(RSS.SIRMC2, lower=1,upper=10,const=opt1$minimum,TS=TS)
  return(list(RSS=opt2$objective,const=opt1$minimum,R0=opt2$minimum))
}

# modeled data that we use to repeatedly generate data with noise
Opt_par <- Opt2$par
names(Opt_par) <- c("const", "R0")
modInfected <- data.frame(ode(y = init, times = day, func = SIR2, parms = Opt_par))$I

# doing the nested model to get RSS
set.seed(1)
Infected_MC <- Infected
modnested <- getOptim()

errrate <- modnested$RSS/sum(Infected) 

MC_R0 <- function(i){
  Infected_MC <- rpois(length(modInfected),modInfected)
  OptMC <- getOptim(TS=Infected_MC)
  return(c(OptMC$const,OptMC$R0))
}

n_mc <- 21


library(parallel)
cl <- makeForkCluster(detectCores()-1)

parLapply(cl,as.list(1:n_mc),MC_R0)

stopCluster(cl)


par <- par[-1,]

plot(par, xlab = "const",ylab="R0",ylim=c(1,5))
title("Monte Carlo simulation")
cov(par)


###conclusion: the parameter R0 can not be reliably estimated

##### End of Monte Carlo estimation


### plotting different values R0

# use the ordinary exponential model to determine const = beta - gamma
const <- coef(mod)[2]
R0 <- 1.1

# graph
plot(-100,-100, xlim=c(0,80), ylim = c(1,N), log="y", 
     ylab = "infected", xlab = "days", yaxt = "n")
axis(2, las=2, at=10^c(0:9),
     labels=c(expression(1),
              expression(10^1),
              expression(10^2),
              expression(10^3),
              expression(10^4),
              expression(10^5),
              expression(10^6),
              expression(10^7),
              expression(10^8),
              expression(10^9)))
axis(2, at=rep(c(2:9),9)*rep(10^c(0:8),each=8), labels=rep("",8*9),tck=-0.02)
title(bquote(paste("scenario's for different ", R[0])), cex.main = 1)

# time
t <- seq(0,60,0.1)

# plot model with different R0
for (R0 in c(1.1,1.2,1.5,2,3,5,10)) {
  fit <- data.frame(ode(y = init, times = t, func = SIR2, parms = c(const,R0)))$I
  lines(t,fit)
  text(t[601],fit[601],
       bquote(paste(R[0], " = ",.(R0))),
       cex=0.7,pos=4)
}

# plot observations
points(day,Infected)