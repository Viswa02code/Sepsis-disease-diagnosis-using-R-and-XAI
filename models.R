library(caTools)
set.seed(123)
data <- read.csv("data.csv")
str(data)

# 1--positive 0--negative

data$Sepsis_Result <- factor(data$Sepsis_Result, levels = c(1, 0),
                                 labels = c(1, 0))
data$Gender <- factor(data$Gender, levels = c(1, 0),
                                 labels = c(1, 0))

table(data$Sepsis_Result)


split <- sample.split(data, SplitRatio = 0.75)
split

Train_data <- subset(data, split == "TRUE")
Test_data <- subset(data, split == "FALSE")

str(Train_data)
str(Test_data)

Train_data1 <- Train_data[1:9]
Test_data1 <- Test_data[1:9]
str(Train_data1)
str(Test_data1)

cluster<-Train_data$Sepsis_Result
cluster
sum(is.na(Train_data1))
sum(is.na(Test_data1))
sum(is.na(cluster))


#knn

library(class)

# Data preparation
k_values <- c(1, 3, 5, 7, 15, 19)

# Calculate accuracy for each k value
model1 <- knn(train = Train_data1, 
              test = Test_data1, 
              cl = cluster, 
              k = 5)

accuracy_values <- sapply(k_values, function(k) {
  model1 <- knn(train = Train_data1, 
                test = Test_data1, 
                cl = cluster, 
                k = k)
  1 - mean(model1 != Test_data$Sepsis_Result)
})

#knn accuracy
library(caret)
library(gmodels)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

CrossTable(x = Test_data$Sepsis_Result,
           y = model1,
           prop.chisq=FALSE)
ac_table<-table(Test_data$Sepsis_Result,model1)
ac_table
accuracy(ac_table)
accuracy_data <- data.frame(K = k_values, Accuracy = accuracy_values)
accuracy_data
xtab <- table(model1, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm <- caret::confusionMatrix(xtab,mode = "everything",
                             positive="1")
cm


#knn Graph

library(ggplot2)


# Plotting
ggplot(accuracy_data, aes(x = K, y = Accuracy*100)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Model Accuracy for Different K Values",
       x = "Number of Neighbors (K)",
       y = "Accuracy") +
  theme_minimal()

#svm

library(e1071)
library(caret)
svm_train <- Train_data[1:10]
svm_test <- Test_data[1:9]

str(svm_test)
str(svm_train)

param_grid <- expand.grid(C = c(0.1, 1),
                          # kernel = c("linear", "radial", "polynomial"),
                          # degree = c(2, 3),
                          # gamma = c(0.1, 1, 10),
                          sigma = c(0.1, 1))

ctrl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

model2 <- train(Sepsis_Result ~ ., data = svm_train, method = "svmRadial", trControl = ctrl,tuneLength = 8,
                   tuneGrid = param_grid)
predictions1 <- predict(model2, newdata = svm_test)

plot(model2)

length(predictions1) == length(Test_data$Sepsis_Result)

print(model2$bestTune)

print(model2)

#svm accuracy

xtab1 <- table(predictions1, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm1 <- caret::confusionMatrix(xtab1,mode = "everything",
                             positive="1")
cm1

#Decision Tree

library(rpart)
library(rpart.plot)
library(ggplot2)


dt_train<-Train_data[1:10]
dt_test<-Test_data[1:9]


model3 = rpart(formula = Sepsis_Result ~ .,
                  data = dt_train,
                  control = rpart.control(minsplit = 1))
predictions2 <- predict(model3, newdata = dt_test, type = "class")
rpart.plot(model3)

#Decision Tree accuracy

length(Test_data$Sepssis)
length(predictions2)
CrossTable(x = Test_data$Sepsis_Result,
           y = predictions2,
           prop.chisq=FALSE)

confusion_matrix <- table(predictions2, Test_data$Sepsis_Result)
print(confusion_matrix)
accuracy(confusion_matrix)

xtab2 <- table(predictions2, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm2 <- caret::confusionMatrix(xtab2,mode = "everything",
                              positive="1")
cm2


#LightGBM

# install.packages("lightgbm")
library(lightgbm)

lgbm_train<-Train_data[1:10]
lgbm_test<-Test_data[1:9]
lgbm_train$Gender <- as.numeric(as.character(lgbm_train$Gender))
lgbm_test$Gender <- as.numeric(as.character(lgbm_test$Gender))
lgbm_train$Sepsis_Result <- as.integer(as.character(lgbm_train$Sepsis_Result))

train_datalgbm <- lgb.Dataset(data = as.matrix(lgbm_train[, -10]), label = lgbm_train$Sepsis_Result)

params <- list(
  objective = "binary",  # Binary classification
  metric = "binary_error",  # Error rate as evaluation metric
  num_leaves = 10,
  learning_rate = 0.1,
  num_iterations = 100
)

model4 <- lgb.train(params, train_datalgbm)

predictions3 <- predict(model4, as.matrix(lgbm_test))

#LGBM Accuracy

predicted_labels <- as.integer(predictions3 > 0.5)
accuracy <- mean(predicted_labels == Test_data$Sepsis_Result)
cat("Accuracy:", accuracy*100, "\n")



bp1 <- factor(predicted_labels, levels = c(0, 1))
ts <- factor(Test_data$Sepsis_Result, levels = c(0, 1))
lgb_cm <- confusionMatrix(bp1, reference = ts)
lgb_precision <- lgb_cm$byClass["Precision"]
lgb_recall <- lgb_cm$byClass["Recall"]
lgb_f1 <- lgb_cm$byClass["F1"]
lgb_accuracy <- lgb_cm$overall["Accuracy"]

print("LightGBM Metrics:")
print(paste("Precision:", lgb_precision))
print(paste("Recall:", lgb_recall))
print(paste("F1 Score:", lgb_f1))
print(paste("Accuracy:", lgb_accuracy))

#LGBM Graph
plot(predictions3)

#xgboost

library(xgboost)

xgboost_train<-Train_data[1:10]
xgboost_test<-Test_data[1:9]

str(xgboost_train)

xgboost_train$Gender <- as.numeric(as.character(xgboost_train$Gender))
xgboost_test$Gender <- as.numeric(as.character(xgboost_test$Gender))
xgboost_train$Sepsis_Result <- as.numeric(as.character(xgboost_train$Sepsis_Result))

train_matrix <- xgb.DMatrix(data = as.matrix(xgboost_train[, c("Gender", "Age", "ICULOS", "HospAdmTime", "HR", "O2Sat", "SBP", "MAP", "DBP")]),
                            label = xgboost_train$Sepsis_Result)

params <- list(
  objective = "binary:logistic",  # For binary classification tasks
  max_depth = 3,
  eta = 0.1
)
model5 <- xgboost(data = train_matrix, params = params, nrounds = 100)
print(model5)
xgboost_test <- xgb.DMatrix(data = as.matrix(xgboost_test))
predictions4 <- predict(model5, newdata = xgboost_test)
plot(predictions4)
print(predictions4)

#XGboost Accuracy


binary_predictions <- ifelse(predictions4 > 0.5, 1, 0)

correct_predictions <- binary_predictions == Test_data$Sepsis_Result

accuracy <- mean(correct_predictions)
accuracy*100

bp <- factor(binary_predictions, levels = c(0, 1))
ts <- factor(Test_data$Sepsis_Result, levels = c(0, 1))

xgb_cm <- confusionMatrix(bp, reference = ts)
xgb_precision <- xgb_cm$byClass["Precision"]
xgb_recall <- xgb_cm$byClass["Recall"]
xgb_f1 <- xgb_cm$byClass["F1"]
xgb_accuracy <- xgb_cm$overall["Accuracy"]

print("XGBoost Metrics:")
print(paste("Precision:", xgb_precision))
print(paste("Recall:", xgb_recall))
print(paste("F1 Score:", xgb_f1))
print(paste("Accuracy:", xgb_accuracy))

#Random forest

library(randomForest)
rf_train<-Train_data[1:10]
rf_test<-Test_data[1:9]
str(rf_test)

hyper_grid <- expand.grid(
  ntree = c(100, 200, 300),  
  mtry = c(2, 4, 6) 
)

ctrl <- trainControl(method = "cv", number = 5)

model6 <- randomForest(Sepsis_Result ~ ., data = rf_train ,method = "rf", 
                       trControl = ctrl, 
                       tuneGrid = hyper_grid)

predictions5 <- predict(model6, rf_test)
importance(model6)

xtab5 <- table(predictions5, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm5 <- caret::confusionMatrix(xtab5,mode = "everything",
                              positive="1")
cm5

#Random Forest Accuracy

confusion_matrix <- table(predictions5, Test_data$Sepsis_Result)
print(confusion_matrix)
accuracy(confusion_matrix)
accuracy <- mean(predictions5== Test_data$Sepsis_Result)
cat("Accuracy:", accuracy*100, "\n")

#RF Graph

plot(model6)

#Ensemble method - Bagging

models <- list(model1,model2,model3,model4,model5,model6)
ctrl <- trainControl(method = "boot")
training<-Train_data[1:10]
testing<-Test_data1  
fit_bagging <- train(Sepsis_Result ~ ., data = training, 
             models = models, 
             trControl = ctrl)
plot(fit_bagging, main = "Bagging Ensemble Method Final Accuracy")

# Predict on the testing set using the bagged ensemble

predictions_bag <- predict(fit_bagging, testing)
confusion_matrix <- table(predictions_bag, Test_data$Sepsis_Result)
cat("Accuracy:", accuracy(confusion_matrix))
xtab6 <- table(predictions_bag, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm6 <- caret::confusionMatrix(xtab6,mode = "everything",
                              positive="1")
cm6

#Ensemble method - Boosting

n.trees <- 100
lr <- 0.1
fit_boosting <- train(Sepsis_Result ~ ., data = training, 
             models = models, 
             trControl = ctrl,
             numTrees = n.trees,
             learningRate = lr)
plot(fit_boosting, main = "Boosting Ensemble Method Final Accuracy")

# Predict on the testing set using the boosting ensemble

predictions_boost <- predict(fit_boosting, testing)
confusion_matrix <- table(predictions_boost, Test_data$Sepsis_Result)
cat("Accuracy:", accuracy(confusion_matrix))
xtab7 <- table(predictions_boost, Test_data$Sepsis_Result)

#confussion matrix
library(caret)
cm7 <- caret::confusionMatrix(xtab7,mode = "everything",
                              positive="1")
cm7

#XAI - LIME

library(lime)

lime_explainer <- lime(rf_train, model6)
class(lime_explainer)
summary(lime_explainer)

explanation <- explain(rf_test[1:5,], lime_explainer, n_labels = 1, n_features = 10)
plot_features(explanation)


lime_explanation <- explain(
  x = rf_test, 
  explainer = lime_explainer, 
  n_permutations = 5000,
  dist_fun = "gower",
  kernel_width = .75,
  n_features = 10, 
  feature_select = "highest_weights",
  labels = "Yes"
)

feature_importance <- lime_explainer$model$importance

# Plot feature importance
barplot(feature_importance, main = "Feature Importance", xlab = "Features", ylab = "Importance")

# Extract feature distribution from the Lime explainer
feature_distribution <- lime_explainer$feature_distribution
plot_features(feature_distribution)
# Plot feature distribution
par(mfrow = c(2, 5)) # Set up multiple plots in a grid
for (i in seq_along(feature_distribution)) {
  barplot(feature_distribution[[i]], main = paste("Feature", i, "Distribution"), xlab = "Value", ylab = "Density")
}

n_bins<-lime_explainer$n_bins

par(mfrow = c(2, 5)) # Set up multiple plots in a grid
for (i in seq_along(n_bins)) {
  barplot(n_bins[[i]], main = paste("Feature", i, "Distribution"), xlab = "Value", ylab = "Density")
}

# Extract bin cuts from the Lime explainer
bin_cuts <- lime_explainer$bin_cuts

# Plot bin cuts
par(mfrow = c(2, 5)) # Set up multiple plots in a grid
for (i in seq_along(bin_cuts)) {
  hist(unlist(bin_cuts[[i]]), main = paste("Bin Cuts for Feature", i), xlab = "Value", ylab = "Frequency")
}

# Plot feature importance using a bar plot
barplot(feature_weights, names.arg = feature_names, main = "Feature Importance (LIME)", xlab = "Features", ylab = "Weight")


str(rf_test)

library(explainer)

library(randomForest)  

shap_explainer <- explain(model6,
                        data = as.data.frame(rf_test),
                        y = Test_data$Sepsis_Result,
                        verbose = FALSE)

shap_explainer <- explainer(model6, data = rf_test, y = "Sepsis_Result")

missing_values <- any(is.na(rf_test))

# Remove rows with missing values
rf_test <- rf_test[complete.cases(rf_test), ]

feature_labels <- names(rf_test)

explanation <- predict_parts(shap_explainer, type = "shap", new_observation = rf_test[1:5, ])
print(explanation)
plot(explanation)

# Compute SHAP explanations
explanation_shap <- predict_parts(rf_explainer, type = "shap", new_observation = rf_test)
plot(explanation_shap)

# Compute Break Down explanations
explanation_break_down <- predict_parts(rf_explainer, type = "break_down", new_observation = rf_test)
plot(explanation_break_down)




