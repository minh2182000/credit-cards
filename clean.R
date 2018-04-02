clean = function(Data){
  # remove missing vars
  out1 = sqldf(paste("select [Card.Name], [Instant..Result.] as Instant,  [Final..Result.] as Final, [Called..Recon.] as Recon, [Credit..Score] as Score, [Average.Age..of.Accounts] as Age, [Stated.Income] as Income, [New.Accounts.Last.3.Months] as New3, [New.Accounts.Last.6.Months] as New6, [New.Accounts.Last.12.Months] as New12",
                     "from Data", 
                     "where Score <> '' ",
                     "and Income <> ''",
                     "and Age <> ''"
  ))
  out1$Card.Name = as.character(out1$Card.Name)
  # change inquiries to 0 if missing
  out1$New3[out1$New3 == "" | is.na(out1$New3)] = 0
  out1$New6[out1$New6 == "" | is.na(out1$New6)] = 0
  out1$New12[out1$New12 == "" | is.na(out1$New12)] = 0
  
  out1$New3 = as.numeric(out1$New3); out1$New6 = as.numeric(out1$New6); out1$New12 = as.numeric(out1$New12)
  
  # change age to months
  for (i in 1:nrow(out1)){
    if (grepl("Year", out1$Age[i])){
      years = strsplit(out1$Age[i], " Year")[[1]][1]
      if (years == "10+"){years = 11} else {years = as.numeric(years)}
    } else {years = 0}
    
    if (grepl("Month", out1$Age[i])){
      months = as.numeric(gsub("\\D", "\\1",
                strsplit(out1$Age[i], "Year")[[1]][length(strsplit(out1$Age[i], " Year")[[1]])]
                          ))
    } else {months = 0}
    out1$Age_month[i] = years * 12 + months
    
  # clean credit score
    out1$Score[i] = regmatches(out1$Score[i],gregexpr('[0-9]+',out1$Score[i]))[[1]][1]
  }
  out1 = out1 [out1$Score <= 850 & out1$Score >=300,]
  out1$Score = as.numeric(out1$Score)
  out1 = out1[!is.na(out1$Score),]
  # change value of result to binary
  out1$Instant = ifelse(out1$Instant == "Approved", 1, 0)
  out1$Final = ifelse(out1$Final == "Approved", 1, 0)

  # change recon to 0/1
  out1$Recon = ifelse(substr(out1$Recon, 1, 2) == "No", 0, 1)
  out1$Recon = (1*(out1$Instant == 0)) * out1$Recon
    
  # take income midpoint
  convert = read.csv("income.csv")
  out2 = sqldf(paste("select out1.*, convert.Convert as [Income.mid]",
                     "from out1 left join convert",
                     "on out1.Income = convert.[Stated.Income]"
  ))
  
  out2 = out2[, -which(colnames(out2) == "Age" | colnames(out2) == "Income")]

  return(na.exclude(out2))
}
