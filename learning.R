load("Train.RData"); source("predict_function.R")
learning = function(bank = "Chase",
                    card = "Freedom",
                    Score = 700,
                    Age_month = 24,
                    Income = "$100,000 - $125,000",
                    New3 = 0,
                    New6 = 0, 
                    New12 = 0
){
    # take income midpoint
    convert = read.csv("income.csv")
    Income.mid = convert$Convert[convert$Stated.Income == Income]
    Data1 = sqldf(paste(
      "select * from credit where Bank = '", bank, "'",
      sep = ""
    ))
    Data = clean(Data1)
    assign("Data", Data, envir = .GlobalEnv)
    
    # get models
    ModelI = Instant_model[[bank]]$model
    ModelF = Final_model[[bank]]$model
    if (is.null(ModelI)){
      pred_I = list(msg_out = "Not enough data for this bank")
      pred_F = list(msg_out = "Not enough data for this bank")
    } else {
      # predict Instant approval
      method = which.max(Accuracies[bank, c(1, 3, 5, 7)])
      newdata = data.frame(Card.Name = card, Instant = 0, Final = 0, Recon = 0, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
      pred_I = predictI(ModelI, newdata = newdata, method)
      
      # predict FInal approval
      method = which.max(Accuracies[bank, c(2, 4, 6, 8)])
      newdata = data.frame(Card.Name = card, Instant = pred_I$pred, Recon = 1, Score = Score, Age_month = Age_month, Income.mid = Income.mid, New3 = New3, New6 = New6, New12 = New12)
      pred_F = predictF(ModelF, newdata = newdata, method)
    }

    # output
    I.out = pred_I$msg_out
    F.out = pred_F$msg_out
    
    return(list(I.out = I.out, F.out = F.out,
                KNN.I.accuracy = Accuracies[bank, "KNN.I"], KNN.F.accuracy = Accuracies[bank, "KNN.F"],
                LR.I.accuracy = Accuracies[bank, "logit.I"], LR.F.accuracy = Accuracies[bank, "logit.F"],
                RF.I.accuracy = Accuracies[bank, "RF.I"], RF.F.accuracy = Accuracies[bank, "RF.F"],
                NN.I.accuracy = Accuracies[bank, "NN.I"], NN.F.accuracy = Accuracies[bank, "NN.F"]
                ))
}
