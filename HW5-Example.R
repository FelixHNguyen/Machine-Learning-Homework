library(mvtnorm)
set.seed(1) #optional; can reproduce my results shown in text
sampsize <- 100
ExpHeightWeight <- c(66,175)
CovMatrix <- matrix(c(7^2,0.5*7*25, 0.5*7*25, 25^2), byrow=TRUE, ncol=2)
X <- rmvnorm(sampsize, mean=ExpHeightWeight, sigma=CovMatrix)


beta_1 <- 0.15
beta_2 <- 0.03
probX <- exp(beta_1 * (X[,1] - 66) + beta_2 * (X[,2] - 175))/(1+exp(beta_1 * (X[,1] - 66) + beta_2 * (X[,2] - 175)))
ISMALE <- (probX > runif(sampsize))
GenderDF1   <- data.frame(ISMALE, HEIGHT = X[ , 1], WEIGHT = X[ , 2])

library(ggplot2)
ggplot(GenderDF1) + geom_text( aes(HEIGHT , WEIGHT , label = ISMALE , colour = factor(ISMALE)))


halfsampsize <- 50
ExpHeightWeightMale <- c(72,190)
ExpHeightWeightFemale <- c(60,160)
XMales <- rmvnorm(halfsampsize,mean=ExpHeightWeightMale,sigma=CovMatrix)
XFemales <- rmvnorm(halfsampsize,mean=ExpHeightWeightFemale,sigma=CovMatrix)
X <- rbind(XMales,XFemales)
ISMALE <- c(rep(TRUE,halfsampsize),rep(FALSE,halfsampsize))
GenderDF2   <- data.frame(ISMALE, HEIGHT = X[ , 1], WEIGHT = X[ , 2])


ggplot(GenderDF2) + geom_text( aes(HEIGHT , WEIGHT , label = ISMALE , colour = factor(ISMALE)))


train <- sample(c(TRUE,FALSE), sampsize, rep=TRUE)
test  <- (!train)


logistic_model_1 <- glm(ISMALE ~ ., data = GenderDF1[train,], family = "binomial")
summary(logistic_model_1)
logistic_probs_1 <- predict(logistic_model_1, GenderDF1[test,], type = "response")
logistic_pred_1  <- rep(FALSE, length(logistic_probs_1))
logistic_pred_1[logistic_probs_1>0.5] <- TRUE
mean(logistic_pred_1 != GenderDF1[test,1])


library(MASS)
lda_model_1 <- lda(ISMALE ~ ., data = GenderDF1[train,])
lda_pred_1  <- predict(lda_model_1, GenderDF1[test,]) 
lda_pred_1  <- lda_pred_1$class
mean(lda_pred_1 != GenderDF1[test,1])

logistic_model_2 <- glm(ISMALE ~ ., data = GenderDF2[train,], family = "binomial")
logistic_probs_2 <- predict(logistic_model_2, GenderDF2[test,], type = "response")
logistic_pred_2  <- rep(FALSE, length(logistic_probs_2))
logistic_pred_2[logistic_probs_2>0.5] <- TRUE
mean(logistic_pred_2 != GenderDF2[test,1])
lda_model_2 <- lda(ISMALE ~ ., data = GenderDF2[train,])
lda_pred_2 <- predict(lda_model_2, GenderDF2[test,]) 
lda_pred_2 <- lda_pred_2$class
mean(lda_pred_2 != GenderDF2[test,1])
qda_model_2 <- qda(ISMALE ~ ., data = GenderDF2[train,])
qda_pred_2 <- predict(qda_model_2, GenderDF2[test,]) 
qda_pred_2 <- qda_pred_2$class
mean(qda_pred_2 != GenderDF2[test,1])


library(class)
train.X <- scale(GenderDF1[train,2])
test.X <- scale(GenderDF1[test,2])
train.Y <- GenderDF1[train,1]
test.Y <- GenderDF1[test,1]
knn.pred1 <- knn(train.X,test.X,train.Y,k=30)
table(knn.pred1,test.Y)
knn.pred2 <- knn(train.X,test.X,train.Y,k=15)
table(knn.pred2,test.Y)
knn.pred3 <- knn(train.X,test.X,train.Y,k=3)
table(knn.pred3,test.Y)