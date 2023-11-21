# Efron's Bootstrap for Sample Median

# R Code 3.1.14.
x <- rcauchy(11, 0, 1)
median(x)
B<-999
mboot <- rep(1:B)
for (j in 1:B){
  mboot[j] <- median(sample(x, replace=T))
}
mean(mboot)-median(x)


