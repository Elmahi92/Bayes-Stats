library(mvtnorm)
library(readxl)

#reading the files 
df<-read_xlsx("Linkoping2022.xlsx")
#Creating the covariate_time vaiable as (the number of days sinvce the beginning of the year / 365)  
a<- df$datetime
#begunning of the year
b<- '2022-01-01'
a<-format.POSIXlt(strptime(a,'%Y-%m-%d'))
b<-format.POSIXlt(strptime(b,'%Y-%m-%d'))
#time diff from the 
x<-as.vector(difftime(a,b,units='days'))
df$cov_tm<-x/365

########
###A####
########
#We assume to use a conjugate prior from the linear regression in Lec 5 , we have been given the prior hyperparamteres as follow:
mu_0= as.matrix(c(0,100,-100),ncol=3)
omega_0=0.01*diag(3)
v_0=1
segma2_0=1
n= length(df$temp)
ndraws=10

# we have the joint prior for beta and segma2 defined as B|Segma2 follows N(muo,sigma2*omegao_inv) and Segma2 follows Inv-Chi(v0,sigma2_0)

#First we draw our random samaple from inv-chi2 using the below defined function from Lec 3 slide 5
# Step 1: Draw X ~ χ²(n - 1)
draw_chi_sq <- function(n) {
  return(rchisq(1, df = n - 1))
}
# Step 2: Compute σ² = (n - 1) * s² / X
compute_sigma_sq <- function(n, segma2_0, X) {
  return((n - 1) * segma2_0 / X)
}

# simulation
segma_estimation <- function(n, mu_0, segma2_0, ndraws) {
  results <- c()
  
  for (i in 1:ndraws) {
    X <- draw_chi_sq(n)
    sigma_sq <- compute_sigma_sq(n, segma2_0, X)
    results[i] <- sigma_sq
  }
  return(results)
}

sigma2<-segma_estimation(n, mu_0, segma2_0, ndraws)

#Now we estimate the betas values using the formula B|Segma2 follows N(muo,sigma2*omegao_inv) and we fit the regression based on 
# temp = beta0 + beta1 time + beta 2 time^2 + erorr

# first we have our error follows the normal distrbution by 0 and 1 

for (i in 1:length(sigma2)) {
  e<- rnorm(1,0,sigma2[i])
  res<-rmvnorm(1,mu_0,sigma2[i]*omega_0)
  temp= x=res[1,1]+res[1,2]*df$cov_tm+res[1,3]*df$cov_tm^2+e
  df[[paste0("temp_p",i)]]<-temp 
}

plt<-ggplot(df,aes(x=cov_tm))+geom_line(aes(y=temp),
                                        color='#FCA311', size=.5)+
  geom_line(aes(y=df[[i]]), color='#14213D',linetype=1)+
  #annotate(geom = "text", x = 8, y = Sd_true,
  #         label = paste0(format(round(Sd_true, 3), nsmall = 3)))+
  labs(x= 'time', y='temp')+
  theme_classic()
plt



# Define a vector of colors
colors <- c("#FCA311", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF",
            "#FF00FF", "#800000", "#008000", "#000080", "#808000",
            "#800080", "#008080", "#808080", "#FFC0CB", "#FFA500",
            "#FFD700", "#A52A2A", "#7FFF00", "#FF1493", "#00BFFF")

# Plot with different colored lines
plt <- ggplot(df, aes(x = cov_tm, y = temp)) +
  geom_line(aes(color = factor('temp')), size = 0.5)

for (i in names(df)[-c(1:4, ncol(df))]) {
  plt <- plt + geom_line(aes_string(y = i, color = factor(i)), linetype = 1)
}

# Map colors to the lines
plt <- plt +
  scale_color_manual(values = colors) +
  labs(x = 'time', y = 'temp',color='Predictions with different Segma values') +
  theme_classic()
plt

