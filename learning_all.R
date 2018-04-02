# Train and save all cards to speed up 
library(sqldf); library(caret)
credit = read.csv("credit.csv")


bank_list = as.character(unique(credit$Bank))
Instant_model = Final_model = list(NULL)
Accuracies = data.frame(logit.I = rep(NA, length(bank_list)),
                        logit.F = rep(NA, length(bank_list)),
                        KNN.I = rep(NA, length(bank_list)),
                        KNN.F = rep(NA, length(bank_list)),
                        RF.I = rep(NA, length(bank_list)),
                        RF.F = rep(NA, length(bank_list)),
                        NN.I = rep(NA, length(bank_list)),
                        NN.F = rep(NA, length(bank_list))
                        )
rownames(Accuracies) = bank_list

i = 0
for (Bank in bank_list){
  print(Bank); i = i + 1
  
  Data1 = sqldf(paste(
    "select * from credit where Bank = '", Bank, "'",
    sep = ""
  ))
  
  Data = clean(Data1)
  if (nrow(Data) <= 5 |
      mean(Data$Instant) == 0 |
      mean(Data$Instant) == 1
  ){
    next
  }
  # Instant Approval: learning with 3 methods
  logit.out.I = logit.I(Data)
  KNN.out.I = KNN.I(Data)
  RF.out.I = RF.I(Data)
  NN.out.I = NN.I(Data)
  best.position = order(c(logit.out.I$accuracy, KNN.out.I$accuracy, RF.out.I$accuracy, NN.out.I$accuracy), decreasing = TRUE)[1]
  best.I = list(logit.out.I, KNN.out.I, RF.out.I, NN.out.I)[[best.position]]
  Instant_model[[i]] = best.I; names(Instant_model)[[i]] = Bank
  
  # Final Approval: learning with 3 methods
  logit.out.F = logit.F(Data)
  KNN.out.F = KNN.F(Data)
  RF.out.F = RF.F(Data)
  NN.out.F = NN.F(Data)
  best.position = order(c(logit.out.F$accuracy, KNN.out.F$accuracy, RF.out.F$accuracy, NN.out.F$accuracy), decreasing = TRUE)[1]
  best.F = list(logit.out.F, KNN.out.F, RF.out.F, NN.out.F)[[best.position]]
  Final_model[[i]] = best.F; names(Final_model)[[i]] = Bank
  
  Accuracies[i,] = c(logit.out.I$accuracy, logit.out.F$accuracy, KNN.out.I$accuracy, KNN.out.F$accuracy,
                     RF.out.I$accuracy, RF.out.F$accuracy, NN.out.I$accuracy, NN.out.F$accuracy)
  
}

save(Accuracies, Instant_model, Final_model, file = "Train.RData")



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


