# Train and save all cards to speed up 
library(sqldf); library(caret)
source("GetChurningData.R")
source("models.R")

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



