GetChurningData = function(){
  credit = read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vToqICtNAq88ygovKl6P26PjnoZj9xgg3OfsQynyrV_Cx-Hh1p9Niz05htkUT8B3VwkFD-F6etL1aOL/pub?gid=0&single=true&output=csv")
  assign("credit", credit, envir = .GlobalEnv)
}
GetChurningData()