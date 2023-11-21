# True parameters
alpha = 2
beta = 3
# Number of draws
n = 11#100000
# Draw a bunch of uniforms
U = runif(n)
# Apply to inverse of the Pareto 
y = alpha/((1-U)^(1/beta))

print(y)

betaGrid = seq(0.1, 10, length=10000)
logLike = n*log(betaGrid) + n*betaGrid*log(alpha) - (1+betaGrid)*sum(log(y))
betaHat = n/(sum(log(y)) - n*(log(alpha)))
sdBetaHat = betaHat/sqrt(n)
plot(betaGrid, logLike, type='l')
abline(v=betaHat,lty=3)

# Number of resamples
B = 1000
# Draw B resamples and store each sample in column of yMat
yMat = matrix(0, nrow=n, ncol=B)
for(i in 1:B) yMat[,i] = sample(y, n, replace=TRUE)
# Compute MLE for each resample (i.e. using each column of yMat)
betaHatBoot = rep(0, length=B)
for(i in 1:B){
  betaHatBoot[i] = n/(sum(log(yMat[,i])) - n*(log(alpha)))
}
muBetaHatBoot = mean(betaHatBoot)
sdBetaHatBoot = sd(betaHatBoot)

print(sdBetaHat)
print(sdBetaHatBoot)
print(c(betaHat + qnorm(0.025)*sdBetaHat, betaHat + qnorm(0.975)*sdBetaHat))
print(c(betaHat + qnorm(0.025)*sdBetaHatBoot, betaHat + qnorm(0.975)*sdBetaHatBoot))
print(c(as.numeric(quantile(betaHatBoot,0.025)), as.numeric(quantile(betaHatBoot,0.975))))









  
