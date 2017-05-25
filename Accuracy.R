#Accuracy Measurements

#Calculation of MAE(Mean Absolute Error)
mae<-0

#count will give total number of ratings predicted
count<-0

#difference matrix gives us (actual rating-predicted rating) for each movie

difference<-EvaluationData-pr

for(i in 1:Test_size){
  for(j in 1:1664){
    if(!is.na(difference[i,j])==TRUE){
      mae<-mae+abs(difference[i,j])  
      count<-count+1
    }
  }
}

mae<-mae/count


#Computing Precision and Recall
#tp = true positive
#fp = false positive
#tn = true negative
#fp = false negative

tp<-0
fp<-0
tn<-0
fn<-0

for(i in 1:E_size){
  for(j in 1:1664){
    if(!is.na(EvaluationData[i,j])==TRUE){
      if(EvaluationData[i,j]>=3&&pr[i,j]>=3){
        tp<-tp+1
      }
      if(EvaluationData[i,j]>=3&&pr[i,j]<3){
        fn<-fn+1
      }
      if(EvaluationData[i,j]<3&&pr[i,j]>=3){
        fp<-fp+1
      }
      if(EvaluationData[i,j]<3&&pr[i,j]<3){
        tn<-tn+1
      }
    }
  }
}

precision<-tp/(tp+fp)
recall<-tp/(tp+fn)

f1<-2*(precision*recall)/(precision+recall)

mae
precision
recall
f1