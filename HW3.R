df <- read.csv("train.csv", header = TRUE)
set.seed(3108)
sample <- sample(nrow(df), 100000)
ourtrain <- df[sample, ]
ourtest <- df[-sample, ]

vars <- colnames(df)[2:131]

ourtrain1 <- model.matrix( ~ .-1, ourtrain[,vars])
ourtest1 <- model.matrix( ~ .-1, ourtest[,vars])


cv.out <- cv.glmnet(x = ourtrain1, y = ourtrain$loss, nfolds = 10, alpha=1, parallel = TRUE)
plot(cv.out)

cv.out$lambda.min
cv.out$lambda.1se

lasso.CVfit <- glmnet(x = ourtrain1,y = ourtrain$loss,alpha=1,lambda=cv.out$lambda.min)

MSE <- sum((ourtest$loss-predict(lasso.CVfit,ourtest1))^2)/88318
RMSE <- sqrt(MSE)