library(lime)      
library(vip)
library(pdp)
library(ggplot2)   
library(caret)  
library(h2o)

h2o.init()


fit_caret <- train(
  Sepsis_Result ~ ., 
  data = data, 
  method = 'ranger',
  trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 1,
  importance = 'impurity'
)
