requirements = c("shiny", "sqldf", "boot", "caret", "class", "e1071", "plotly", "randomForest", "nnet")
for (r in requirements){
    if (!r %in% rownames(installed.packages())){ install.packages(r, repos = "https://cloud.r-project.org") }
}