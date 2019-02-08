load("Project/Data/models_declaration.RData") # contains classifiers declaration
load("Project/Data/preprocessed.RData")
load("Project/Data/nn_result_default.RData")
load("Project/Data/dt_result_default.RData")
load("Project/Data/svm_result_default.RData")
#load("Project/Data/results_default.RData) # for the future
## Use variable importance from Decision Tree to select variables
## to be used in KNN which is prone to overfitting

# Variable importance from the final decision tree model
imps <- as.matrix(varImp(dt_model)$importance)
# How many variable ?
threshold <- 1:100
nimps <- c()
for (t in threshold) {
  nimps <- c(nimps, length(imps[imps >= t]))
}
plot(threshold, nimps, type = "l",
     main = "Number of variables vs importance threshold")

t <- 57
var <- rownames(data.frame(imps[imps > t,]))
# Fit KNN on most important variables only
knn_model <- train_knn_classifier(train_df[, var], metric, control)

