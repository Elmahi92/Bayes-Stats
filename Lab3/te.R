library(LaplacesDemon)
library(ggplot2)
library(bayesutils)
library(gridExtra)
library(rstan)
set.seed(123456)
data <- as.data.frame(readRDS("Precipitation.rds"))
n <- nrow(data)
names(data) <- c("precipitation")
v_0 <- 1
sigma_0 <- var(data$precipitation)
sigma <- sigma_0
tau_0 <- 1
1
Enligt anmälande lärare har 
studenterna skrivit fel rubrik - 
  borde stå Lab3
Markeringarna visar likheter med L4-phiho267-zijfe244.pdf
mu_0 <- mean(data$precipitation)
mu <- rnorm(n = 1, mean = mu_0, tau_0)
nDraws <- 1000
Gibbs1 <- data.frame("mu" = numeric(), "sigma" = numeric())
Gibbs2 <- data.frame("mu" = numeric(), "sigma" = numeric())
mean_vec <- vector(mode = "numeric", length = nDraws)
variance_vec <- vector(mode = "numeric", length = nDraws)
for(i in 1:nDraws){
  w <- (n/sigma)/((n/sigma) + (1/tau_0))
  mu_n <- w * mean(data$precipitation) + (1 - w) * mu_0
  tau_n <- 1 / (n/sigma + 1/tau_0)
  mu <- rnorm(n = 1, mean = mu_n, sd = tau_n)
  old_sigma <- sigma
  v_n <- v_0 + n
  second_term <- (v_0 * sigma_0 + sum((data$precipitation - mu)ˆ2))/ (n + v_0)
  sigma <- rinvchisq(n = 1, df = v_n, scale = second_term)
  Gibbs2 <- rbind(Gibbs2, data.frame(mu, sigma))
  if(i==1){
    sampling <- data.frame("mu" = mu, "sigma" = sigma)
  }else{
    sampling <- data.frame("mu" = c(mu, mu), "sigma" = c(old_sigma, sigma))
  }
  Gibbs1 <- rbind(Gibbs1, sampling)
}