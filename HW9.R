df <- read.csv("wine-1.csv", header=FALSE)
head(df)
colnames(df) <- c("y","x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13")
df$y <- as.factor(df$y) # Change to factor

#Rescale
df_rescale <- as.data.frame(scale(df[,2:14], center = TRUE, scale = TRUE))
df_rescale1 <- as.data.frame(df$y)
colnames(df_rescale1) <-c("y")
df <- as.data.frame(c(df_rescale,df_rescale1))

#Split Train/Test
sample <- sample.int(n = nrow(df), size = 150, replace = F)
train <- df[sample, ]
test  <- df[-sample, ]
mynet <- nnet(y ~ ., data = train, size = 10, maxit = 200)
mypreds <- predict(mynet, test)
mypreds <- as.data.frame(mypreds)
mypreds$max <- apply(mypreds, 1, FUN=max)

mypreds$result <- ifelse(mypreds[,1] == mypreds[,4], 1, ifelse(mypreds[,2] == mypreds[,4], 2, 3))

table(mypreds$result, test$y)