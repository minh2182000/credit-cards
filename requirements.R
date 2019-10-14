requirements = c("shiny", "sqldf", "boot", "caret", "class", "e1071", "plotly", "randomForest", "nnet")
need_to_install = NULL
for (r in requirements){
    if (!r %in% rownames(installed.packages())){ 
    	need_to_install = c(need_to_install, r)
    }
}

install.packages(need_to_install, repos = "https://cloud.r-project.org")
