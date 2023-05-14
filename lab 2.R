
# First we want to calculate the vale of beta and the Jacopiang inv of beta by using thge optim function and the code from the lec notes
# Note that we have tau = 2 and prior beta follows N(0,tau^2I)

### Select Logistic or Probit regression and install packages ###
Probit <- 0

### Prior and data inputs ###
Covs <- c(2:8) # Select which covariates/features to include
standardize <- F # If TRUE, covariates/features are standardized to mean 0 and variance 1
lambda <- 2 # scaling factor for the prior of beta in our case tau = 2

# Loading out data set
wat<-read.table("WomenAtWork.dat",header = T) # read data from file
Nobs <- dim(wat)[1] # number of observations
y <- wat[1] # y=1 if the women is working, otherwise y=0.
X <- as.matrix(wat[,Covs]) # Covs matrix 7*7
Xnames <- colnames(X)
# Standraizing the covs matrix
if (standardize){
  Index <- 2:(length(Covs)-1)
  X[,Index] <- scale(X[,Index])
}
Npar <- dim(X)[2]

#############################################################################################
# This is to add y variable as binary response and adding intercept, for now it's not needed
# for (ii in 1:Nobs){
#   if (wat$quality[ii] > 5){
#     y[ii] <- 1
#   }
# }
# wat <- data.frame(intercept=rep(1,Nobs),wat) # add intercept
#############################################################################################3

# Setting up the prior
mu <- as.matrix(rep(0,Npar)) # Prior mean vector
Sigma <- (lambda)^2 *diag(Npar) # Prior covariance matrix

# Functions that returns the log posterior for the logistic and probit regression.
# First input argument of this function must be the parameters we optimize on, 
# i.e. the regression coefficients beta.

LogPostLogistic <- function(betas,y,X,mu,Sigma){
  linPred <- X%*%betas;
  logLik <- sum( linPred*y - log(1 + exp(linPred)) );
  # if (abs(logLik) == Inf){
  #              logLik = -20000
  #              }# Likelihood is not finite, stear the optimizer away from here!
  logPrior <- dmvnorm(betas, mu, Sigma, log=TRUE);
  
  return(logLik + logPrior)
}

# Not in use we change the value to 0 at the beginning of the code
################################################################################
LogPostProbit <- function(betas,y,X,mu,Sigma){
  linPred <- X%*%betas;
  SmallVal <- .Machine$double.xmin
  logLik <- sum(y*log(pnorm(linPred)+SmallVal) + (1-y)*log(1-pnorm(linPred)+SmallVal))
  logPrior <- dmvnorm(betas, mu, Sigma, log=TRUE);
  return(logLik + logPrior)
}
##############################################################################
# Select the initial values for beta
initVal <- matrix(0,Npar,1)

if (Probit==1){
  logPost = LogPostProbit;
} else{
  logPost = LogPostLogistic;
}

# The argument control is a list of options to the optimizer optim, where fnscale=-1 means that we minimize 
# the negative log posterior. Hence, we maximize the log posterior.  
OptimRes <- optim(initVal,logPost,gr=NULL,y,X,mu,Sigma,method=c("BFGS"),control=list(fnscale=-1),hessian=TRUE)

# Values of betas

OptimRes$par[,1]

# Printing the Hessian Matrix

print(OptimRes$hessian)



# Printing the results to the screen
names(OptimRes$par) <- Xnames # Naming the coefficient by covariates
approxPostStd <- sqrt(diag(solve(-OptimRes$hessian))) # Computing approximate standard deviations.
names(approxPostStd) <- Xnames # Naming the coefficient by covariates
print('The posterior mode is:')
print(OptimRes$par)
print('the Hessian Matrix:')
print(OptimRes$hessian)
print('The approximate posterior standard deviation is:')
print(approxPostStd)

# Now we compare with the results from the regresion model: glmModel<- glm(Work ~ 0 + ., data = WomenAtWork, family = binomial)

glmModel<- glm(Work ~ 0 + ., data = wat, family = binomial)
summary(glmModel)

# Now we Compute an approximate 95% equal tail posterior probability interval for the regression coerffcient to the variable NSmallChild

# We use the function rmvnorm to generate the variates using OptimRes$par as our mean and approxPostStd as sigma 
postmode<-as.matrix(OptimRes$par[,1])
poststd<- -solve(OptimRes$hessian)
watvar<-data.frame(rmvnorm(n=2,mean = postmode, sigma = poststd))

#For a 95% CI, you would typically calculate the lower and upper bounds at quantiles 0.025 and 0.975, respectively.
print('An approximate 95% equal tail posterior probability interval for the regression coeffcient to the variable NSmallChild is:')
print(quantile(watvar[,6],c(0.025,.975)))


###B
pred_prob<- function(ndraws,x_new){
        x_new<-as.matrix(x_new,ncol=1)
        betas<-rmvnorm(n=ndraws,mean = postmode, sigma = poststd)
        pr_y<-data.frame(x=betas%*%x_new)
        pr_y$x_logit<-1/(1+exp(-pr_y$x))
        plt <- ggplot(pr_y,aes(x = x_logit)) +geom_histogram(aes(y=..density..),
                linetype=1,
                fill='#14213D')+
                geom_density(alpha=.15,color="#FCA311",size=1,fill="#FCA311")+
                labs(x='Pr(y=0|x)',y=' ',)
        plt
}