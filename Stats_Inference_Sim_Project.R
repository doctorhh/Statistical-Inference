set.seed(10)

# set lambda to 0.2
lambda <- 0.2

# 40 samples
Num_Sample <- 40

# 1000 simulations
Num_Sim <- 1000

# simulate 1000 instances of 40 exponentials
Sim_Exp <- replicate(Num_Sim, rexp(Num_Sample, lambda))

# calculate mean of the sample of exponentials
mean_Sim_Exp <- apply(Sim_Exp, 2, mean)

# calculate the mean of the sample mean
mean_Sample <- mean(mean_Sim_Exp)

# calculate the thoeritical mean
mean_Theory <- 1/lambda

# Represent the sample mean distribution
hist(mean_Sim_Exp, breaks=50, xlab = "Mean", main = "Sample mean distributions")
abline(v = mean_Sample , col = "blue")
abline(v = mean_Theory, col = "orange")
legend('topright', c("Simulation mean", "Theoretical mean"),
       lty=c(1,1), col=c("blue", "orange"))

# calculate the standard deviation of the sample distribution
stddev_Sample <- sd(mean_Sim_Exp)

# calculate the  theoritical standard deviation
stddev_Theory <- (1/lambda)/sqrt(Num_Sample)

# calcultate the variance of the sample distribution
var_Sample <- stddev_Sample^2

# variance from analytical expression
var_Theory <- ((1/lambda)*(1/sqrt(Num_Sample)))^2


hist(mean_Sim_Exp, breaks=50, prob=TRUE,
     main="Density Simulation",
     xlab="")

# calculate and mark the density of the sample mean
lines(density(mean_Sim_Exp), col="blue")

# mark the mean of the sample mean
abline(v=mean(mean_Sim_Exp), col="blue")
# mark the theoretical mean
abline(v=1/lambda, col="orange")

# theoretical density of the averages of samples
xfit <- seq(min(mean_Sim_Exp), max(mean_Sim_Exp), length=100)
yfit <- dnorm(xfit, mean=mean_Theory, sd=stddev_Theory)
lines(xfit, yfit, pch=22, col="orange", lty=2)

# add legend
legend('topright', c("Simulation distribution", "Theoretical distribution"),
       lty=c(1,2), col=c("blue", "orange"))

# compare the distribution of 1000 means (simulation) of 40 exponentials to a normal distribution
qqnorm(mean_Sim_Exp)
qqline(mean_Sim_Exp, col = 2)
