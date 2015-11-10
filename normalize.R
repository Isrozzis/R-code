#Normalize function
#doesn't work with NA or zeroes

normalize <- function(x){
  if(x==0){
    return (0)
  }
  else{
    return ( (x - min(x)) / (max(x) - min(x))) 
  }
}
