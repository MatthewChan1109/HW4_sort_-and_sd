# �ŧi exchange.sort.asc() ���
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

# ���X�@���H���V�q
unsorted_vector <- round(runif(10) * 100)

# ����H���V�q������Ƨ�

exchange.sort.asc(unsorted_vector,TRUE)
# ����H���V�q�����W�Ƨ�
exchange.sort.asc(unsorted_vector,FALSE)
# ����H���V�q�H�ۭq�禡�p��зǮt
selfsd(unsorted_vector)
# ����H���V�q�H�зǨ禡�w�p��зǮt
sd(unsorted_vector)