#Prediction Function

Prediction<-function(x,y){
  U<-na.omit(predictset[x,])
  num<-0
  den<-0
  ans<-0

  test_mov <- NULL
  
  for(mov in 1:1664){
    if(!is.na(Test[x,mov]))
      test_mov <- c(test_mov,mov)
  }
  
  for(k in test_mov){
      sum1<-0
      sum2<-0
      for(tuser in U){
        if(!is.na(TrainD[tuser,k])){
          if(!is.na(TrainD[tuser,y])){
            sum1 = sum1 + abs(TrainD[tuser,k]*TrainD[tuser,q])
            sum2 = sum2 + abs(TrainD[tuser,k])
          }
        }
        if(!is.na(cross_cosine[tuser,x]))
          sum1 <- sum1 + sum1*cross_cosine[tuser,x]*groupsim[x,networkof[tuser,1]]
        if(!is.na(groupsim[x,networkof[tuser,1]]))  
          sum2 <- sum2 + sum2*groupsim[x,networkof[tuser,1]]
      }
      num <- num + Test[x,k]*sum1
      den <- den + Test[x,k]*sum2
  }
  ans <- Test_avg[x,1] + Test_sd[x,1]*num/den
  if(is.na(ans))
    ans=Test_avg[x,1]
  return(ans)
}


#Prediction of user ratings in Evaluation Data
#predicted is copy of Evaluation Data



E_size = dim(EvaluationData)[1]
pr <- EvaluationData

for(i in 1:E_size)
{
  for(j in 1:1664){
    if(!is.na(pr[i,j])){
      pr[i,j]<-Prediction(i,j)
    }
  }
}
