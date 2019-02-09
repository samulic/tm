library(caret)
library(dplyr)
library(tictoc)
library(doParallel)

### MODELS TRAINING AND TESTING ###

folder <- "Project/" # Project's root folder
## Classifiers used
# Decision Tree
train_dt_classifier <- function(train_df, metric, control) {
  library("C50")
  # Start timer...
  tic("Decision Tree")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="C5.0", metric=metric, trControl=control)
  # Stop timer...
  toc()
  return(model)
  detach("package:C50", unload = TRUE)
}
# Support Vector Machine
train_svm_classifier <- function(train_df, metric, control) {
  tic("SVM")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="svmRadial", metric=metric, trControl=control)
  toc()
  return(model)
}
# K-Nearest Neighbors
train_knn_classifier <- function(train_df, metric, control) {
  tic("KNN")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="knn", metric=metric, trControl=control)
  toc()
  return(model)
}
# Random Forestl
train_rf_classifier <- function(train_df, metric, control) {
  tic("Random Forest")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="rf", metric=metric, trControl=control)
  toc()
  return(model)
}
# Neural Networks
train_nn_classifier <- function(train_df, metric, control) {
  tic("Neural Networks")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="nnet", metric=metric, 
                 trControl=control, MaxNWts = 20000)
                 #tuneGrid = nnet_grid)
  toc()
  return(model)
}
# NN 2 hidden layers
train_nn2_classifier <- function(train_df, metric, control) {
  tic("Neural Networks 2")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="mlpWeightDecayML", metric=metric, 
                 trControl=control, tuneGrid = nnet2_grid, MaxNWts = 50000)
  toc()
  return(model)
}

## Load preprocessed dataframes from '2_preprocessing_6class.R'
# train_df and test_df, type of matrix (i.e. "tfidf")
load(paste0(folder, "Data/preprocessed_00_6topic_trainDuplicates.RData"))
# Summarize target class distributions
prop.table(table(train_df$Topic)) * 100
prop.table(table(test_df$Topic)) * 100

# Use parallel processing
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

# CARET: train control parameters and desired performance metric
control <- trainControl(method = "cv", number = 5, classProbs = TRUE) #,
                        #summaryFunction = twoClassSummary) # for ROC metric
metric <- "Accuracy"#"ROC" #"Accuracy" alternatively

nnet_grid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6), 
                         .size = c(3, 5, 10, 20))
nnet2_grid <- expand.grid(.decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6),
                          .layer1 = c(3, 5, 10, 20), .layer2 = c(3, 5, 10, 20), 
                          .layer3 = 0)

# Classifiers
dt_model <- train_dt_classifier(train_df, metric, control)
save(list = c("dt_model"), file = "Project/Data/06_dt_result.RData")
svm_model <- train_svm_classifier(train_df, metric, control)
save(list = c("svm_model"), file = "Project/Data/06_svm_result.RData")
knn_model <- train_knn_classifier(train_df, metric, control, 
                                  tuneGrid = expand.grid(.k = 2:7))
save(list = c("knn_model"), file = "Project/Data/06_knn_result.RData")
nn_model  <- train_nn_classifier(train_df, metric, control)
save(list = c("nn_model"), file = "Project/Data/06_nn_result.RData")



nn2_model <- train_nn2_classifier(train_df, metric, control)
save(list = c("nn2_model"), file = "Project/Data/06_nn2hl_result.RData")
rf_model <- train_rf_classifier(train_df, metric, control)

# Stop parallel processing
stopCluster(cl)

results <- resamples(list(DecisionTree = dt_model, 
                          RandomForest = rf_model, 
                          SVM = svm_model, 
                          KNN = knn_model,
                          NeuralNetwork = nn_model))
# summarize the performances' distribution
summary(results)
# boxplots of results
bwplot(results)
# dot plots of results
dotplot(results)

# Test predictions
dt_predictions <- predict(dt_model, newdata = test_df)
dt_confusion_matrix <- confusionMatrix(table(dt_predictions, test_df$Topic))
cat('Decision Tree test accuracy: ', unname(dt_confusion_matrix$overall[1]), '\n')

svm_predictions <- predict(svm_model, newdata = test_df)
svm_confusion_matrix <- confusionMatrix(table(svm_predictions, test_df$Topic))
cat('SVM test accuracy: ', unname(svm_confusion_matrix$overall[1]), '\n')

knn_predictions <- predict(knn_model, newdata = test_df)
knn_confusion_matrix <- confusionMatrix(table(knn_predictions, test_df$Topic))
cat('KNN test accuracy: ', unname(knn_confusion_matrix$overall[1]), '\n')

rf_predictions <- predict(rf_model, newdata = test_df)
rf_confusion_matrix <- confusionMatrix(table(rf_predictions, test_df$Topic))
cat('Random Forest test accuracy: ', unname(rf_confusion_matrix$overall[1]), '\n')

nn_predictions <- predict(nn_model, newdata = test_df)
nn_confusion_matrix <- confusionMatrix(table(nn_predictions, test_df$Topic))
cat('Neural Networks test accuracy: ', unname(nn_confusion_matrix$overall[1]), '\n')

