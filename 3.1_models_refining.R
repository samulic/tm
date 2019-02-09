library(tictoc)
library(caret)
load("Project/Data/models_declaration.RData") # contains classifiers declaration
load("Project/Data/preprocessed.RData")
load("Project/Data/nn_result_default.RData")
load("Project/Data/dt_result_default.RData")
load("Project/Data/svm_result_default.RData")
#load("Project/Data/results_default.RData) # for the future
## Use variable importance from Decision Tree to select variables
## to be used in KNN which is prone to overfitting
train_knn_classifier <- function(train_df, metric, control) {
  tic("KNN")
  set.seed(7)
  model <- train(Topic~., data=train_df, method="knn", metric=metric, 
                 trControl=control, tuneGrid = expand.grid(.k = 2:4))
  toc()
  return(model)
}
# Variable importance from the final decision tree model
imps <- as.matrix(varImp(dt_model)$importance)
# How many variable ?
threshold <- 15:50
nimps <- c()
for (t in threshold) {
  nimps <- c(nimps, length(imps[imps >= t]))
}
plot(threshold, nimps, type = "l",
     main = "Number of variables vs importance threshold")

t <- 57
var <- rownames(data.frame(imps[imps > t,]))
var <- c(var, "Topic") # add target class

# Fit KNN on most important variables only
small_knn_model <- train_knn_classifier(train_df[, var], metric, control)

knn_model_357 <- small_knn_model
#knn_model_358 <- small_knn_model # t = 57
# t = 22

results <- resamples(list(n358 = knn_model_358, 
                          n357 = knn_model_357))
bwplot(results)
