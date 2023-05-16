
RWMSampler<- function(logPostFunc,nDraws,c,y,x,mu,Sigma){
  # First we buld our data frame of samples
  sample <- data.frame(matrix(nrow = nDraws, ncol = ncol(x)))
  colnames(sample) <- colnames(x)
  # The inital sample value c here represent a tuning parameter
  sample[1,] <- mvrnorm(1, posteriorMode, c*postCov)
  # Now we implement the Metropolis-Hastings in which we generate samples from the proposal distribution in this case 
  # We look at the results of the first sample as theta_i-1 pluged in mvnorm to get theta_i and the we use the values in
  # our proposed logPostFunc
  i=1
  while (i < nDraws) {
    theta_old<-as.numeric(sample[i,])
    theta_new<-mvrnorm(1,theta_old,c*postCov)
  # We define th our accept/reject threshold 
    th<-runif(1,0,1)
  # now we find the value of the target/proposed distribution
    proposed<- logPostFunc(theta_new,y = y,
                          x = x,
                          mu = posteriorMode,
                          Sigma = postCov)
    target<- logPostFunc(theta_old,y = y,
                        x = x,
                        mu = posteriorMode,
                        Sigma = postCov)
    # the ratio of posterior densities in the Metropolis acceptance probability
    if (th<min(1,exp(proposed-target))) {
      i=i+1
      sample[i,]<-theta_new
    }
  }
 return(sample)
}

nDraws=1000
c=.5

res <- RWMSampler(logPostFunc = logPossion,
                                nDraws = nDraws,
                                  c=c,
                                  y = y,
                                  x = x,
                                  mu = posteriorMode,
                                  Sigma = postCov)


names<-colnames(res)
p_fun<- function(coln){
  plt <- ggplot(res,aes_string(x = coln)) +
    geom_histogram(aes(y=..density..),linetype=1,fill='#14213D',bins = 20)+
    geom_density(alpha=.2,color="#FCA311",size=1,fill="#FCA311")
  plt
}

plot(arrangeGrob(grobs = lapply(names, p_fun)))



#First we estimate the betas from our RWMSampler function 
betas<- as.matrix(res)
# Input data 
x_new <- as.matrix(c(1,1,0,1,0,1,0,1.2,0.8))
# calculating the probability
prob<-data.frame(x=exp(betas  %*% x_new))

# histogram of the probabilities
ggplot(prob,aes(x = x)) +
  geom_histogram(aes(y=..density..),linetype=1,fill='#14213D',bins = 20)+
  geom_density(alpha=.2,color="#FCA311",size=1,fill="#FCA311")+
  annotate(geom = "text", x = .8, y = 5,
           label = paste0("The probability of no 
                          bidders in this new auction =",format(round(mean(x), 3), nsmall = 3)))+
  labs(x = 'Probabilities', y = 'density',
           title ='Plot of the predictive distribution')

