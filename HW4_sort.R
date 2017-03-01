# 宣告 exchange.sort.asc() 函數
exchange.sort.asc <- function(input_vector,decreasing=FALSE) {
  input_vector_clone <- input_vector
  vector_length <- length(input_vector)
  #increasing
  if (decreasing==FALSE){
      (for (i in 1:(vector_length - 1)) {
        for (j in (i + 1):vector_length) {
          if (input_vector[i] > input_vector[j]) {
            temp <- input_vector[i]
            input_vector[i] <- input_vector[j]
            input_vector[j] <- temp
          }
        }
      })}
  #decreasing
  else{for (i in 1:(vector_length - 1)) {
    for (j in (i + 1):vector_length) {
      if (input_vector[i] <input_vector[j]) {
        temp <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- temp
      }
    }
  }
    
  }
 
  return(input_vector)
}
#######################################################################
selfsd <- function(input_vector) {
  #input_vector_clone <- input_vector
  vector_length <- length(input_vector)
  n<-vector_length
  sum1=0
  
  ####sum and  average #####
  for (i in 1:(n)){
    sum1=sum1+input_vector[i]
  }
  average=sum1/n
  sumsd=0
  for (i in 1:(n)){
    sumsd=sumsd+(input_vector[i]-average)^2
  }
  return(sqrt(sumsd/(n-1)))
    
}

# 產出一組隨機向量
unsorted_vector <- round(runif(10) * 100)

# 對該隨機向量做遞減排序

exchange.sort.asc(unsorted_vector,TRUE)
# 對該隨機向量做遞增排序
exchange.sort.asc(unsorted_vector,FALSE)
# 對該隨機向量以自訂函式計算標準差
selfsd(unsorted_vector)
# 對該隨機向量以標準函式庫計算標準差
sd(unsorted_vector)
