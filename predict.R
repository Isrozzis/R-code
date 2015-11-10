#generates a confusion matrix for this data set. Default data set is air, which is the name i'm using in R
#the function will only work for this data set since our binary variable is "event"

predict.prob <- function(model, data=air){
  
  b <- c()
  a <- predict(model, data, type="response")
  for (i in 1:length(a)){
    if(a[i]==0){
      b[i] = 0
    }
    else{
      b[i] <- ( (a[i] - min(a)) / (max(a) - min(a))) 
    }
  }
  
  pred_prob <- rep(0, length(a))
  pred_prob[b > .5] <- 1
  
  table(pred_prob, data$event)
  
  
}