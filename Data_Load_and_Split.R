#Loading the library
library(recommenderlab)

data("MovieLense")

#MovieLense
Movie <- MovieLense
Movie<-as(Movie,"matrix")


#To find the number of movies rated by each user: count(943x1 matrix)
count <- matrix(nrow=943,ncol=1)
for(i in 1:943){
  count[i] <- 0
  for(j in 1:1664){
    if(!is.na(Movie[i,j])){
      count[i] <- count[i] + 1 
    }
  }
}

Movie<-MovieLense

#Splitting in Train and Test dataset
scheme <- evaluationScheme(Movie,method="split",train=0.8,given=-10)

Train <- getData(scheme,"train")
Train <- as(Train,"matrix")

Test <- getData(scheme,"known")
Test <- as(Test,"matrix")

EvaluationData <- getData(scheme,"unknown")
EvaluationData <- as(EvaluationData,"matrix")
