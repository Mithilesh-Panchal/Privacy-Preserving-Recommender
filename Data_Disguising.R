#Disguising the Train Data

Train_size <- dim(Train)[1]

#To count the number of movies rated by each user in Training set: countTrain
countTrain <- matrix(nrow=Train_size,ncol=1)

Trainrownames <- rownames(Train)

for(i in 1:Train_size){
  countTrain[i] <- 0
  for(j in 1:1664){
    if(!is.na(Train[i,j])){
      countTrain[i] <- countTrain[i] + 1 
    }
  }
}

#To find average of ratings given by each user in Training set: Train_avg
Train_avg <- rowMeans(Train,na.rm = TRUE)
Train_avg <- as(Train_avg,"matrix")

#To find standard deviation of ratings for each userin Training set: Train_sd
Train_sd <- matrix(nrow=Train_size,ncol=1)
for(i in 1:Train_size){
  Train_sd[i] <- var(Train[i,],na.rm = TRUE)
  Train_sd[i] <- sqrt(Train_sd[i])
}

#Computing the z-score matrix for Training set: Train_z
Train_z <- matrix(nrow=Train_size,ncol=1664)
for(i in 1:Train_size){
  for(j in 1:1664){
    if(!is.na(Train[i,j])){
      Train_z[i,j] <- (Train[i,j] - Train_avg[i])/Train_sd[i]
    }
  }
}

#The server will decide range of random numbers [-alpha,alpha] for Data Disguising
alpha <- 1

#Disguising the user rating using random number addition in range [-alpha,alpha]
TrainD <- matrix(nrow=Train_size,ncol=1664)

for(i in 1:Train_size){
  n <- runif(countTrain[i],-alpha,alpha)
  k<-1
  for(j in 1:1664){
    if(!is.na(Train[i,j])){
      TrainD[i,j] = Train_z[i,j] + n[k]
      k <- k + 1
    }
  }
}


#Disguising the Test Data
#Testing Phase

Test_size <- dim(Test)[1]

#To count the number of movies rated by each user in Testing set: countTest
countTest <- matrix(nrow=Test_size,ncol=1)

Testrownames <- rownames(Test)

for(i in 1:Test_size){
  countTest[i] <- 0
  for(j in 1:1664){
    if(!is.na(Test[i,j])){
      countTest[i] <- countTest[i] + 1 
    }
  }
}

#To find average of ratings given by each user in Testing set: Test_avg

Test_avg <- rowMeans(Test,na.rm = TRUE)
Test_avg <- as(Test_avg,"matrix")

#To find standard deviation of ratings for each user in Testing set: Test_sd

Test_sd <- matrix(nrow=Test_size,ncol=1)
for(i in 1:Test_size){
  Test_sd[i] <- var(Test[i,],na.rm = TRUE)
  Test_sd[i] <- sqrt(Test_sd[i])
}

#Computing the z-score matrix for Testing set: Test_z

Test_z <- matrix(nrow=Test_size,ncol=1664)
for(i in 1:Test_size){
  for(j in 1:1664){
    if(!is.na(Test[i,j])){
      Test_z[i,j] <- (Test[i,j] - Test_avg[i])/Test_sd[i]
    }
  }
}

#The server will decide range of random numbers [-alpha,alpha] for Data Disguising
alpha <- 1

#Disguising the user rating using random number addition in range [-alpha,alpha]
TestD <- matrix(nrow=Test_size,ncol=1664)

for(i in 1:Test_size){
  n <- runif(countTest[i],-alpha,alpha)
  k<-1
  for(j in 1:1664){
    if(!is.na(Test[i,j])){
      TestD[i,j] = Test_z[i,j] + n[k]
      k <- k + 1
    }
  }
}