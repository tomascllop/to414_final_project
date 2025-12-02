library(randomForest)
library(pROC)
library(caret)

## ---- Random Forest model ----
rf <- randomForest(
  as.factor(goal) ~ .,
  data      = train_dt,
  ntree     = 1500,
  mtry      = floor((ncol(train_dt) - 1) / 3),
  importance = TRUE
)

print(rf)
varImpPlot(rf)


## ---- Predict probabilities on test set ----
rf_pred <- predict(rf, test_dt, type = "prob")[, "1"]   # prob of class "1"


## ---- ROC curve ----
roc_curve <- roc(
  response = factor(test_dt$goal, levels = c(0, 1)),
  predictor = rf_pred,
  levels = c(0, 1)
)

plot(roc_curve)


## ---- Best threshold from ROC ----
best_thresh <- coords(roc_curve, "best", ret = "threshold")
best_thresh


## ---- Confusion matrix ----
cm_rf <- confusionMatrix(
  rf_bin_pred,
  factor(test_dt$goal, levels = c(0, 1)),
  positive = "1"
)

cm_rf
