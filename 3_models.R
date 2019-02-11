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
  model <- train(Topic~., data=train_df, method="nnet", metric=metric, trControl=control)
  toc()
  return(model)
}

## Load preprocessed dataframes from '2_preprocessing.R'
# train_df and test_df, type of matrix (i.e. "tfidf")
load(paste0(folder, "Data/preprocessed.RData"))
# Summarize target class distributions
prop.table(table(train_df$Topic)) * 100
prop.table(table(test_df$Topic)) * 100

# Use parallel processing
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

# CARET: train control parameters and desired performance metric
control <- trainControl(method = "cv", number = 5, classProbs = TRUE,
                        summaryFunction = twoClassSummary) # for ROC metric
metric <- "ROC" #"Accuracy" alternatively

# save(list = c("control", "metric", "train_nn_classifier",
#               "train_dt_classifier", "train_dt_classifier",
#               "train_knn_classifier", "train_svm_classifier"),
#      file = paste0(folder, "Data/models_declaration.RData"))
# Classifiers
nn_model <- train_nn_classifier(train_df, metric, control)
dt_model <- train_dt_classifier(train_df, metric, control)
svm_model <- train_svm_classifier(train_df, metric, control)
rf_model <- train_rf_classifier(train_df, metric, control)
knn_model <- train_knn_classifier(train_df, metric, control)

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
dt_predictions_prob <- predict(dt_model, newdata = test_df, type = "prob")
dt_confusion_matrix <- confusionMatrix(table(dt_predictions, test_df$Topic))
cat('Decision Tree test accuracy: ', unname(dt_confusion_matrix$overall[1]), '\n')

svm_predictions <- predict(svm_model, newdata = test_df)
svm_predictions_prob <- predict(svm_model, newdata = test_df, type = "prob")
svm_confusion_matrix <- confusionMatrix(table(svm_predictions, test_df$Topic))
cat('SVM test accuracy: ', unname(svm_confusion_matrix$overall[1]), '\n')

knn_predictions <- predict(knn_model, newdata = test_df)
knn_predictions_prob <- predict(knn_model, newdata = test_df, type = "prob")
knn_confusion_matrix <- confusionMatrix(table(knn_predictions, test_df$Topic))
cat('KNN test accuracy: ', unname(knn_confusion_matrix$overall[1]), '\n')

nn_predictions <- predict(nn_model, newdata = test_df)
nn_predictions_prob <- predict(nn_model, newdata = test_df, type = "prob")
nn_confusion_matrix <- confusionMatrix(table(nn_predictions, test_df$Topic))
cat('Neural Networks test accuracy: ', unname(nn_confusion_matrix$overall[1]), '\n')


## ROC Curve ( for binary Topic )
for_lift <- data.frame(Class = test_df$Topic,
                      DT = dt_predictions_prob$Sci,
                      SVM = svm_predictions_prob$Sci,
                      #KNN = knn_predictions_prob$Sci,
                      NN  = nn_predictions_prob$Sci)
#lift <- lift(Class ~ DT + SVM + KNN + NN, data = for_lift, )
lift <- lift(Class ~ DT + SVM + NN, data = for_lift, class = "Sci")

library(ggplot)

ggplot(lift$data) + 
  geom_line(aes(1 - Sp , Sn, color = liftModelVar), size = 1.5) +
  scale_color_discrete(guide = guide_legend(title = "Classifier")) +
  theme(text = element_text(size=15)) +
  labs(y="Sens", x = "1 - Spec") +
  ggtitle("ROC curves on test set")

