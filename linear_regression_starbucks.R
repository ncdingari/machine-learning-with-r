library(ggcorrplot)
star= read.csv('starbucks.csv')
colnames(star)

m1 = lm(Calories~Fat+Carb+Fiber+Protein, data=star)
summary(m1)
gvlma::gvlma(m1)
par(mfrow=c(2,2))
plot(m1)

corr = round(cor(star), 2)
ggcorrplot(corr, hc.order = T, type = "lower",
outline.col = "white",lab=T,
ggtheme = ggplot2::theme_gray,
colors = c("#C93756", "white", "#26828e"))

m2 = lm(Calories~Fat+Carb+Protein, data=star)
summary(m2)
gvlma::gvlma(m2)
par(mfrow=c(2,2))
plot(m2)

m3 = lm(Calories~Fat+Carb+Protein, data=star[-c(33,55,60,63,83,89,91,98,99,102,103,104,105,110),])
gvlma::gvlma(m3)

par(mfrow=c(2,2))
plot(m3)

star1 = star[-c(33,55,60,63,83,89,91,98,99,102,103,104,105,110),]
set.seed(2020)
trainInd = sample(nrow(star1), 0.8*nrow(star1))
trainDF = star1[trainInd,]
testDF = star1[-trainInd,]

head(trainDF)
head(testDF)

hist(trainDF$Calories)
hist(testDF$Calories)
plot(density(trainDF$Calories))
plot(density(testDF$Calories))


model4 = lm(Calories~Fat+Carb+Protein, data=trainDF)
summary(model4)
gvlma::gvlma(model4)

predTrain = predict(model4, trainDF)
predTest = predict(model4, testDF)

sqrt(mean((predTrain-trainDF$Calories)^2))
sqrt(mean((predTest-testDF$Calories)^2))

1 - sum((trainDF$Calories - predTrain)^2)/sum((trainDF$Calories - mean(trainDF$Calories))^2)
1 - sum((testDF$Calories - predTest)^2)/sum((testDF$Calories - mean(testDF$Calories))^2)

par(mfrow=c(1,1))
plot(trainDF$Calories, predTrain)
plot(testDF$Calories, predTest)

newDF = star[sample(nrow(star), 0.1*nrow(star)),]
predNew = predict(model4, newDF)

sqrt(mean((predNew-newDF$Calories)^2))
1 - sum((newDF$Calories - predNew)^2)/sum((newDF$Calories - mean(newDF$Calories))^2)

newDF1 = star[sample(nrow(star1), 0.1*nrow(star1)),]
predNew1 = predict(model4, newDF1)

sqrt(mean((predNew1-newDF1$Calories)^2))
1 - sum((newDF1$Calories - predNew1)^2)/sum((newDF1$Calories - mean(newDF1$Calories))^2)
             
