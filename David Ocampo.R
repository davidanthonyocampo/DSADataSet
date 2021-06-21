#By: David Ocampo
#DSA Data Set Case Study

bankDataSet <- read.csv("C:/Users/david/Desktop/DSA Data Set.csv")

#fix skewed data for "pdays" and "previous"
bankDataSet$pdays[bankDataSet$pdays == 999] <- NA
bankDataSet$previous[bankDataSet$previous == 0 ] <- NA
summary(bankDataSet)

#take out columns
bankDataSet <- bankDataSet[-c(1,4,8,9,10,11,13,14,17,19,20,21)]

#create test and train data for generalized linear model
set.seed(100)
ind <- sample(2,nrow(bankDataSet),replace = TRUE,prob = c(.7,.3))
train.data <- bankDataSet[ind == 1, ]
test.data <- bankDataSet[ind == 2, ]

#scatterplot matrix for multicollinearity test. Took out cons.price.idx due and euribor3m
data = data.frame(test.data)
plot(data)

#initial look at model variables
bankFormula <- y~.
model <- glm(bankFormula,family=binomial,data=train.data)
summary(model)

#including only variables with p < .05
bankFormula <- y~job+default+campaign+poutcome+emp.var.rate+cons.conf.idx
model <- glm(bankFormula,family=binomial,data=train.data)
summary(model)

#show stats for new model
trainTablePredictions <- table(round(model$fitted.values),train.data$y)
trainTableAccuracy <- (trainTablePredictions[1] + trainTablePredictions[4])/sum(trainTablePredictions)
testTablePredictions <- table(round(predict.glm(model,test.data, type="response")),test.data$y)
testTableAccuracy <- (testTablePredictions[1] + testTablePredictions[4])/sum(testTablePredictions)