##############################################################
# Report
# © Leonardo Lavagna
# July 2021
##############################################################



# IMPORT DEI DATI
dati <- read.delim2("./misure_1.txt", header = TRUE, 
                    sep = "", dec = ",", stringsAsFactors = FALSE)
dati <- dati[, -c(1)]
y_obs <- as.numeric(dati$peso_finale)

# ANALISI ESPLORATIVA

# stimatori puntuali
mu <- round(mean(y_obs),4)
sigma2 <-  round(var(y_obs),4)
sigma <- round(sqrt(var(y_obs)),4)
M <- round(median(y_obs),4)
m <-y_obs[which.max(tabulate(match(y_obs, unique(y_obs))))]
mu
sigma

# grafici
plot(a, col=rgb(0,0,1,1/4), xlim=c(0.5,0.7))  
plot(b, col=rgb(1,0,0,1/4), xlim=c(0.5,0.7), add=T)

plot(x_i, y_obs, xlab="x_i", ylab="y_obs", pch = 19, main = "")
abline(lm(y_obs ~ x_i), col = "red", lwd = 3)
grid()

# correlazione
r <- cor(y_obs,x_i)
r
n <- 50
(1-r^2)/(sqrt(n-2))

# ANALISI FREQUENTISTA
alpha= 0.05
mu + qt(1 - alpha / 2, length(y_obs) - 1) *
  sigma / sqrt(length(y_obs)) * c(-1, 1)

# rispetto al valore soglia s=0.25
alpha= 0.05
paste(qt(1 - alpha / 2, length(y_obs) - 1) * sigma / sqrt(length(y_obs)))

# ANALISI BAYESIANA
library(TeachingDemos)
# Distribuzione a priori
mu_0 <- mu
sigma2_0 <- var(y_obs)
sigma2 <- 0.025
n <- length(y_obs)

# Distribuzione a posteriori 
mu_n<-(1/sigma2_0 * mu_0 + n/sigma2 * mean(y_obs)) /(1/sigma2_0 + n/sigma2) 
sigma2_n<-1/(1/sigma2_0 + n/sigma2) 

# Mediana a posteriori
round(qnorm(0.5, mu_n, sigma2_n),6)
round(mu_n,6)
round(mean(y_obs),6)

#HPD
hpd(posterior.icdf=qnorm,mean=mu_n,sd=sqrt(sigma2_n),conf=0.95)

# Equal tail
alpha_conf <- 0.95
q_lower <- qnorm(alpha_conf / 2, mean=mu_n,sd=sqrt(sigma2_n))
q_upper <- qnorm(1-alpha_conf / 2, mean=mu_n,sd=sqrt(sigma2_n))
c(round(q_lower,6), round(q_upper,6))

# probabilità intervallo
A <- 0.5756
B<- 0.5771

p_prior <- pnorm(B, mu_0, sigma2_0)-pnorm(A,mu_0, sigma2_0)
p_prior

p_posterior <- pnorm(B, mu_n, sigma2_n)-pnorm(A,mu_n, sigma2_n)
p_posterior
