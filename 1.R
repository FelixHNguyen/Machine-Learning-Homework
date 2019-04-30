getwd()
setwd('C:/Users/phhun/Documents/Courses/')
d <- read.table("asciiqob.txt", header = FALSE)
colnames(d) <- c("lwklywge", "educ","yob","qob","pob")
summary(d)
d1 <- d[1:200000,]
d2 <-d[200001:329509,]
m1 <- lm(lwklywge ~ educ, data = d1)
summary(m1)
yhat_IS_1 <- predict(m1,d1)
OLS_IS_MSE_1 <- sum((d1$y - yhat_IS_1)^2)/200000
OLS_IS_MSE_1
yhat_OOS_1 <- predict(m1,d2)
OLS_OOS_MSE_1 <- sum((d2$y - yhat_OOS_1)^2)/129509
OLS_OOS_MSE_1

====
  
m2 <- lm(lwklywge ~ yob, data = d1)
summary(m2)
yhat_IS_2 <- predict(m2,d1)
OLS_IS_MSE_2 <- sum((d1$y - yhat_IS_2)^2)/200000
OLS_IS_MSE_2
yhat_OOS_2 <- predict(m2,d2)
OLS_OOS_MSE_2 <- sum((d2$y - yhat_OOS_2)^2)/129509
OLS_OOS_MSE_2

=====
  
m3 <- lm(lwklywge ~ qob, data = d1)
summary(m3)


m4 <- lm(lwklywge ~ pob, data = d1)
summary(m4)

m5 <- lm(lwklywge ~ educ + yob + qob + pob, data = d1)
summary(m5)