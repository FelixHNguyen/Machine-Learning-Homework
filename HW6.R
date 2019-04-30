install.packages("gam")
install.packages("pROC")

Packages <- c("gam", "ggplot2", "sp", "splines","boot","glmnet","pROC")

lapply(Packages, library, character.only = TRUE)

#Fitting GAM - DIY #1 

#a. Generating Data

set.seed(3108)
X1 <- rnorm(100)
X2 <-rnorm(100)
eps <- rnorm(100, sd = 0.3)
Y <- 0.8 + 3.1 * X1 + 1.93 * X2 + eps

#b. Initializing B1

beta1 <- 8

#c. 

z <- Y - beta1*X1

fit1 <- lm(z ~ X2)
beta2 <- fit1$coef[2]

#d.
z_b <- Y - beta2*X2
fit2 <- lm(z_b ~ X1)
beta1 <- fit2$coef[2]

#f.


beta0 <-  rep(0, 1000)
beta1 <-  rep(0, 1000)
beta2 <-  rep(0, 1000)

for (i in 1:1000) {
  z <- Y - beta1[i] * X1
  beta2[i] <- lm(z ~ X2)$coef[2]
  z <- Y - beta2[i] * X2
  beta0[i] <- lm(z ~ X1)$coef[1]
  beta1[i+1] <- lm(z ~ X1)$coef[2]
}

require(reshape2)
df <- data.frame(Iteration=1:1000, beta0, beta1=beta1[-1], beta2)
melted_df <- melt(df, id.vars="Iteration")
ggplot(melted_df, aes(x=Iteration, y=value, group=variable, col=variable)) + 
  geom_line(size=1) + ggtitle("Estimate by iteration")

#e.

fit.lm <- lm(Y ~ X1 + X2)
coef(fit.lm)
plot(beta0, type="l", col="red", lwd=2, xlab="Iterations", 
     ylab="beta estimates", ylim=c(0,4))
lines(beta1[-1], col="green", lwd=2)
lines(beta2, col="blue", lwd=2)
abline(h=coef(fit.lm)[1], lty="dashed", lwd=3, col="brown")
abline(h=coef(fit.lm)[2], lty="dashed", lwd=3, col="darkgreen")
abline(h=coef(fit.lm)[3], lty="dashed", lwd=3, col="darkblue")
legend(x=600,y=3, c("beta0", "beta`", "beta2", "multiple regression"),
       lty = c(1,1,1,2), 
       col = c("red","green","blue","gray"))

#EXAMPLE

library(ISLR)