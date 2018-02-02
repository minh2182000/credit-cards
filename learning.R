
credit = read.csv("credit.csv")
learning = function(bank = "Chase",
                    card = "Freedom",
                    Score = 700,
                    Age_month = 24,
                    Income = "$100,000 - $125,000",
                    New3 = 0,
                    New6 = 0, 
                    New12 = 0
){
  print(paste(bank,
              card,
              Score,
              Age_month,
              Income,
              New3,
              New6, 
              New12, sep = ","))
  # take income midpoint
  convert = read.csv("income.csv")
  Income.mid = convert$Convert[convert$Stated.Income == Income]
  
  # get data of the bank
    Data1 = sqldf(paste(
      "select * from credit where Bank = '", bank, "'",
      sep = ""
    ))
    
    Data = clean(Data1)
    assign("Data", Data, envir = .GlobalEnv)
    print("pass1")
    
    # learn & predict
    print("LR")
    logit.out.I = logit.I(Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    logit.out.F = logit.F(logit.out.I$pred, Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    print("KNN")
    KNN.out.I = KNN.I(Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    KNN.out.F = KNN.F(KNN.out.I$pred, Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    print("RF")
    RF.out.I = RF.I(Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    RF.out.F = RF.F(KNN.out.I$pred, Data, card, Score, Age_month, Income.mid, New3, New6, New12)
    
    print("pass2")
    
    # find best method
    best.position = order(c(logit.out.I$accuracy, KNN.out.I$accuracy, RF.out.I$accuracy), decreasing = TRUE)[1]
    best.I = list(logit.out.I, KNN.out.I, RF.out.I)[[best.position]]
    best.position = order(c(logit.out.F$accuracy, KNN.out.F$accuracy, RF.out.F$accuracy), decreasing = TRUE)[1]
    best.F = list(logit.out.F, KNN.out.F, RF.out.F)[[best.position]]
    
    # output
    I.out = best.I$msg_out
    F.out = best.F$msg_out
    
    print("done learning")
    return(list(I.out = I.out, F.out = F.out,
                KNN.I.accuracy = KNN.out.I$accuracy, KNN.F.accuracy = KNN.out.F$accuracy,
                LR.I.accuracy = logit.out.I$accuracy, LR.F.accuracy = logit.out.F$accuracy,
                RF.I.accuracy = RF.out.I$accuracy, RF.F.accuracy = RF.out.F$accuracy
                ))
}


# --------------------logistics regression  ----------------------------------------------
# Instant approval:
logit.I = function(Data,
                       Card.Name = "Freedom",
                       Score = 700,
                       Age_month = 24,
                       Income.mid =50000,
                       New3 = 0,
                       New6 = 0, 
                       New12 = 0
                       
){
  model = step(glm(as.factor(Instant) ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12, data = Data, family = binomial), trace = FALSE)
  cv = train(model$formula, data = Data, method = "glm", family = binomial, trControl = trainControl("cv", 3, savePredictions = TRUE))
  accuracy = cv$results$Accuracy
  xnew = data.frame(Card.Name = Card.Name, Instant = 0, Final = 0, Recon = 0, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  
  pred = predict(model, newdata = xnew, type = "link", se.fit = TRUE)
  fit = model$family$linkinv(pred$fit)
  up = pred$fit + 1.645*pred$se.fit; up = model$family$linkinv(up)
  low = pred$fit - 1.645*pred$se.fit; low = max(0, model$family$linkinv(low))
  msg_out = paste("Predicted probability of instant approval: ", round(low*100), "% - ", round(up*100), "%", sep = "")
  return(list(model = model, accuracy = accuracy, msg_out = msg_out, pred = fit))
}

# Final approval:
logit.F = function(Instant.pred,
                   Data,
                   Card.Name = "Freedom",
                   Score = 700,
                   Age_month = 24,
                   Income.mid =50000,
                   New3 = 0,
                   New6 = 0, 
                   New12 = 0,
                   Recon = 1
                   
){
  Data$Final = as.factor(Data$Final)
  model = step(glm(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon, data = Data, family = binomial), trace = FALSE)
  cv = train(model$formula, data = Data, method = "glm", family = binomial, trControl = trainControl("cv", 3, savePredictions = TRUE))
  accuracy = cv$results$Accuracy
  
  xnew = data.frame(Card.Name = Card.Name, Instant = Instant.pred, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  pred = predict(model, newdata = xnew, type = "link", se.fit = TRUE)
  up = pred$fit + 1.645*pred$se.fit; up = model$family$linkinv(up)
  low = pred$fit - 1.645*pred$se.fit; low = max(0, model$family$linkinv(low))
  msg_out = paste("Predicted probability of Final approval: ", round(low*100), "% - ",  round(up*100), "%", sep = "")
  return(list(model = model, accuracy = accuracy, msg_out = msg_out))
}

#----------------- KNN ---------------------------------------------------------
# Instant approval
KNN.I = function(Data,
                 Card.Name = "Freedom",
                 Score = 700,
                 Age_month = 24,
                 Income.mid =50000,
                 New3 = 0,
                 New6 = 0, 
                 New12 = 0
){
  Data$Instant = as.factor(Data$Instant)
  ctrl <- trainControl(method="cv", 3)
  cv = train(Instant ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12,
            data = Data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),
            tuneGrid=data.frame(.k=1:min(20, nrow(Data)/2)), prob = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  xnew = data.frame(Card.Name = Card.Name, Instant = 0, Final = 0, Recon = 0, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  pred = predict(cv, newdata = xnew, type = "prob")
  msg_out = paste("Predicted probability of Instant approval: ", round(pred[,"1"]*100),"%", sep = "")
  return(list(model = cv, accuracy = accuracy, msg_out = msg_out, pred = pred[,"1"] ))
}
# Final approval
KNN.F = function(Instant.pred,
                 Data,
                 Card.Name = "Freedom",
                 Score = 700,
                 Age_month = 24,
                 Income.mid =50000,
                 New3 = 0,
                 New6 = 0, 
                 New12 = 0,
                 Recon = 1
){
  Data$Final = as.factor(Data$Final)
  ctrl <- trainControl(method="cv", 3)
  cv = train(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon,
             data = Data, method = "knn", trControl = ctrl, preProcess = c("center", "scale"),
             tuneGrid=data.frame(.k=1:min(20, nrow(Data)/2)), prob = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  xnew = data.frame(Card.Name = Card.Name, Instant = Instant.pred, Final = 0, Recon = 1, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  pred = predict(cv, newdata = xnew, type = "prob")
  msg_out = paste("Predicted probability of Final approval: ", round(pred[,"1"]*100),"%", sep = "")
  return(list(model = cv, accuracy = accuracy, msg_out = msg_out ))
}

# ------------------------- RANDOM FOREST ----------------------------
RF.I = function(Data,
                 Card.Name = "Freedom",
                 Score = 700,
                 Age_month = 24,
                 Income.mid =50000,
                 New3 = 0,
                 New6 = 0, 
                 New12 = 0
){
  Data$Instant = as.factor(Data$Instant)
  ctrl <- trainControl(method="cv", 3)
  cv = train(Instant ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12,
             metric = "Accuracy", data = Data, method = "rf", trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  xnew = data.frame(Card.Name = Card.Name, Instant = 0, Final = 0, Recon = 0, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  pred = predict(cv, newdata = xnew, type = "prob")
  msg_out = paste("Predicted probability of Instant approval: ", round(pred[,"1"]*100),"%", sep = "")
  return(list(model = cv, accuracy = accuracy, msg_out = msg_out, pred = pred[,"1"] ))
}
# Final approval
RF.F = function(Instant.pred,
                 Data,
                 Card.Name = "Freedom",
                 Score = 700,
                 Age_month = 24,
                 Income.mid =50000,
                 New3 = 0,
                 New6 = 0, 
                 New12 = 0,
                 Recon = 1
){
  Data$Final = as.factor(Data$Final)
  ctrl <- trainControl(method="cv", 3)
  cv = train(Final ~ Card.Name + Score + Age_month + Income.mid + New3 + New6 + New12 + I(1-Instant):Recon,
             metric = "Accuracy", data = Data, method = "rf", trControl = ctrl, 
             norm.vote = TRUE)
  accuracy = max(cv$results$Accuracy)
  
  xnew = data.frame(Card.Name = Card.Name, Instant = Instant.pred, Final = 0, Recon = 1, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
  pred = predict(cv, newdata = xnew, type = "prob")
  msg_out = paste("Predicted probability of Final approval: ", round(pred[,"1"]*100),"%", sep = "")
  return(list(model = cv, accuracy = accuracy, msg_out = msg_out ))
}
