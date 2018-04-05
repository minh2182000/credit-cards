

# --------------------logistics regression  ----------------------------------------------
# Instant approval:
logit.I = function(Data
){
  model = step(glm(as.factor(Instant) ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12, data = Data, family = binomial), trace = FALSE)
  if (length(all.vars(model$formula)) == 1){
    model = glm(as.factor(Instant) ~ Score + Age_month, data = Data, family = binomial)
  }
  cv = train(model$formula, data = Data, method = "glm", family = binomial, trControl = trainControl("cv", 10, savePredictions = TRUE))
  accuracy = cv$results$Accuracy
  return(list(model = model, accuracy = accuracy))
}

# Final approval:
logit.F = function(Data
){
  Data$Final = as.factor(Data$Final)
  model = step(glm(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon, data = Data, family = binomial), trace = FALSE)
  if (length(all.vars(model$formula)) == 1){
    model = glm(as.factor(Final) ~ I(1-Instant):Recon + Score + Age_month, data = Data, family = binomial)
  }
  cv = train(model$formula, data = Data, method = "glm", family = binomial, trControl = trainControl("cv", 3, savePredictions = TRUE))
  accuracy = cv$results$Accuracy
  
  return(list(model = model, accuracy = accuracy))
}

#----------------- KNN ---------------------------------------------------------
# Instant approval
KNN.I = function(Data
){
  Data$Instant = as.factor(Data$Instant)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Instant ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12,
             data = Data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),
             tuneGrid=data.frame(.k=1:min(20, nrow(Data)/2)), prob = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  return(list(model = cv, accuracy = accuracy))
}
# Final approval
KNN.F = function(Data
){
  Data$Final = as.factor(Data$Final)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon,
             data = Data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),
             tuneGrid=data.frame(.k=1:min(20, nrow(Data)/2)), prob = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  return(list(model = cv, accuracy = accuracy))
}

# ------------------------- RANDOM FOREST ----------------------------
RF.I = function(Data
){
  Data$Instant = as.factor(Data$Instant)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Instant ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12,
             metric = "Accuracy", data = Data, method = "rf", trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  return(list(model = cv, accuracy = accuracy))
}
# Final approval
RF.F = function(Data
){
  Data$Final = as.factor(Data$Final)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon,
             metric = "Accuracy", data = Data, method = "rf", trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  return(list(model = cv, accuracy = accuracy))
}

# -----------neural network -------------------------
# ------------------------- RANDOM FOREST ----------------------------
NN.I = function(Data
){
  Data$Instant = as.factor(Data$Instant)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Instant ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12,
             metric = "Accuracy", data = Data, method = "nnet", trace = FALSE, trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  return(list(model = cv, accuracy = accuracy))
}
# Final approval
NN.F = function(Data
){
  Data$Final = as.factor(Data$Final)
  ctrl <- trainControl(method="cv", 10)
  cv = train(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon,
             metric = "Accuracy", data = Data, method = "nnet", trace = FALSE, trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  return(list(model = cv, accuracy = accuracy))
}
