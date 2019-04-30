library(MASS)
data("Boston")
df <- Boston

head(df)

#Polynomial
m_poly <- lm(nox ~ poly(dis,3), data = df)

summary(m_poly)

dis1 <- seq(from = min(df$dis), to = max(df$dis), by = 0.001)
yhatpoly <- predict(m_poly,list(dis=dis1))
plot(df$nox ~ df$dis)
lines(dis1, yhatpoly)

#Ten Polynomial
library(boot)
library(lmvar)
set.seed(3108)
rss <- rep(0,10)
plot(df$nox ~ df$dis)
for (i in 1:10){
  m_poly2 <- lm(nox ~ poly(dis,i), data = df, x=TRUE, y=TRUE)
  yhatpoly <- predict(m_poly2,list(dis=dis1))
  rss[i] <- c(crossprod(m_poly2$residuals))
  lines(dis1, yhatpoly, col = 25*i + 1)
}
rss

#10-folds validation polynomial
cverr <- rep(0,10)
for (i in 1:10){
  m_poly3 <- glm(nox ~ poly(dis,i), data = df, x=TRUE, y=TRUE)
  cverr[i] <- cv.glm(df,m_poly3, K=10)$delta[1]
}
plot(1:10,cverr)
text(1:10,cverr,labels=round(cverr,4))

m_poly3 <- glm(nox ~ poly(dis,3), data = df, x=TRUE, y=TRUE)
cverr00 <- cv.glm(df,m_poly3, K=10)

#Splines:

library(splines)
median(df$dis) == attr(bs(df$dis,df=4), "knots")
m_spline <- lm(nox ~ bs(dis,df = 4), data = df)
summary(m_spline)
yhatsp <- predict(m_spline,list(dis=dis1))
plot(df$nox ~ df$dis)
lines(dis1, yhatsp)


#10 Splines
set.seed(3108)
rss1 <- rep(0,10)
plot(df$nox ~ df$dis)
for (i in 3:12){
  m_spline2 <- lm(nox ~ bs(dis,df = i), data = df)
  yhatsp <- predict(m_spline2,list(dis=dis1))
  rss1[i-2] <- c(crossprod(m_spline2$residuals))
  lines(dis1, yhatsp, col = 25*(i-2) + 1)
}
rss1

#CV

set.seed(3108)
cverr1 <- rep(0,10)
for (i in 3:12){
  m_spline3 <- glm(nox ~ bs(dis,df = i), data = df)
  cverr1[i-2] <- cv.glm(df,m_spline3, K=10)$delta[1]
}
plot(1:10,cverr1)

