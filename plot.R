myplot = function(card = "Freedom",
                  Score = 700,
                  Age_month = 24,
                  Income = "$100,000 - $125,000"){
  Data = get("Data", envir = .GlobalEnv)
  Data = Data[Data$Card.Name == card,]
  Data$Final = ifelse(Data$Final == 1, "Approved", "Denied")
  convert = read.csv("income.csv")
  Income.mid = convert$Convert[convert$Stated.Income == Income]
  
  newrow = nrow(Data) + 1
  Data[newrow,] = NA
  Data$Score[newrow] = Score
  Data$Age_month[newrow] = Age_month
  Data$Income.mid[newrow] = Income.mid
  Data$Final[newrow] = "You"
  
  
  plot_out = plot_ly(x = Data$Score, y = Data$Age_month, z = Data$Income.mid, type = "scatter3d", color = Data$Final, colors = c("black", "red", "blue")) %>%
    layout(title = "Where You Are In Our Data",
           scene = list(
             xaxis = list(title = "Credit Score"),
             yaxis = list(title = "Average credit age (month)"),
             zaxis = list(title = "Income")
           ))
  return(plot_out)
}

