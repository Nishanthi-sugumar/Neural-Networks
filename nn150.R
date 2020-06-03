library(neuralnet)
library(nnet)
library(NeuralNetTools)
startups <- read.csv(file.choose())
View(startups)
str(startup)

startups$State <- as.numeric(revalue(startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startups)
startups<-as.data.frame(startups)
attach(startups)


#EDA
plot(R.D.Spend,Profit)
plot(Administration,Profit)
plot(Marketing.Spend,Profit)
plot(State,Profit)
windows()
pairs(startups)
cor(startups)
summary(startups)

#applying normalization technique for whole dataset
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(startups,FUN=normalize))
summary(startups_norm$Profit) 
summary(startups$profit)

# Data Partition 
set.seed(123)
ind <- sample(2, nrow(startups_norm), replace = TRUE, prob = c(0.7,0.3))
startups_train <- startups_norm[ind==1,]
startups_test  <- startups_norm[ind==2,]


# Creating a neural network model on training data


startups_model <- neuralnet(Profit~R.D.Spend+Administration
                            +Marketing.Spend+State,data = startups_train)
str(startups_model)
plot(startups_model, rep = "best")

summary(startups_model)

par(mar = numeric(4), family = 'serif')
plotnet(startups_model, alpha = 0.6)


# Evaluating model performance

set.seed(12323)
model_results <- compute(startups_model,startups_test[1:4])
predicted_profit <- model_results$net.result

# Predicted profit Vs Actual profit of test data.
cor(predicted_profit,startups_test$Profit)

# since the prediction is in Normalized form, we need to de-normalize it 
# to get the actual prediction on profit
str_max <- max(startups$Profit)
str_min <- min(startups$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_profit,str_min,str_max)
head(ActualProfit_pred)
# Improve the model performance :
set.seed(12345)
startups_model2 <- neuralnet(Profit~R.D.Spend+Administration
                             +Marketing.Spend+State,data = startups_train,
                             hidden = 2)
plot(startups_model2 ,rep = "best")
summary(startups_model2)

model_results2<-compute(startups_model2,startups_test[1:4])
predicted_Profit2<-model_results2$net.result
cor(predicted_Profit2,startups_test$Profit)
plot(predicted_Profit2,startups_test$Profit)
par(mar = numeric(4), family = 'serif')
plotnet(startups_model2, alpha = 0.6)
