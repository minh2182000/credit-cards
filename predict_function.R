predictI = function(model, newdata, method){
  if (method == 1){
    pred = predict(model, newdata = newdata, type = "link", se.fit = TRUE)
    fit = model$family$linkinv(pred$fit)
    up = pred$fit + 1.645*pred$se.fit; up = model$family$linkinv(up)
    low = pred$fit - 1.645*pred$se.fit; low = max(0, model$family$linkinv(low))
    msg_out = paste("Predicted probability of instant approval: ", round(low*100), "% - ", round(up*100), "%", sep = "")
    return(list(msg_out = msg_out, pred = fit))
  }
  if (method == 2){
    pred = predict(model, newdata = newdata, type = "prob")
    msg_out = paste("Predicted probability of Instant approval: ", round(pred[,"1"]*100),"%", sep = "")
    return(list(msg_out = msg_out, pred = pred[,"1"] ))
  }
  if (method == 3){
    pred = predict(model, newdata = newdata, type = "prob")
    msg_out = paste("Predicted probability of Instant approval: ", round(pred[,"1"]*100),"%", sep = "")
    return(list(msg_out = msg_out, pred = pred[,"1"] ))
  }
  if (method == 4){
    pred = predict(model, newdata = newdata, type = "raw")
    msg_out = paste("Predicted probability of Instant approval: ", round(pred*100),"%", sep = "")
    return(list(msg_out = msg_out, pred = pred))
  }  
}

predictF = function(model, newdata, method){
  if (method == 1){
    pred = predict(model, newdata = newdata, type = "link", se.fit = TRUE)
    up = pred$fit + 1.645*pred$se.fit; up = model$family$linkinv(up)
    low = pred$fit - 1.645*pred$se.fit; low = max(0, model$family$linkinv(low))
    msg_out = paste("Predicted probability of Final approval: ", round(low*100), "% - ",  round(up*100), "%", sep = "")
    return(list(msg_out = msg_out))
    
  }
  if (method == 2){
    pred = predict(model, newdata = newdata, type = "prob")
    msg_out = paste("Predicted probability of Final approval: ", round(pred[,"1"]*100),"%", sep = "")
    return(list(msg_out = msg_out ))
  }
  if (method == 3){
    pred = predict(model, newdata = newdata, type = "prob")
    msg_out = paste("Predicted probability of Final approval: ", round(pred[,"1"]*100),"%", sep = "")
    return(list(msg_out = msg_out ))
  }
  if (method == 4){
    pred = predict(model, newdata = newdata, type = "prob")
    msg_out = paste("Predicted probability of Final approval: ", round(pred[,"1"]*100),"%", sep = "")
    return(list(msg_out = msg_out ))
  }  
  
}