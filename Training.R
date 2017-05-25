#Training Phase

#Improving the accuracy by using AIS
#Computing Cosine Similarity using Disguised Rating Matrix

TD <- as(TrainD,"realRatingMatrix")
cosine<-similarity(TD,TD,method = "cosine")
cosine<-as(cosine,"matrix")

#Establishing the Immune Network
initimmune=matrix(nrow=10,ncol=Train_size-10)

#taking a set of 943 users
users<-seq(from=1, to=Train_size, by=1)

#selecting a random number
a<-sample(users,1)

#the first immune network n1
initimmune[1,1]<-a
rem<-c(a)
users<-setdiff(users,rem)
k<-2

#generation of initial immune network
for(m in 1:9){
  small<-100000
  worst<-0
  for(i in 1:m){
    for(j in users){
      if(identical((small-cosine[j,initimmune[i,1]])>0,TRUE)==TRUE){
        small<-cosine[j,initimmune[i,1]]
        worst<-j
      }
    }
  }
  initimmune[k,1]<-worst
  rem<-c(worst)
  users<-setdiff(users,rem)
  k<-k+1
}


#Expanding to the FINAL IMMUNE NETWORK
#size(10X1) matrix denoting number of antibodies in each immune network
size<-matrix(nrow=10,ncol=1)
for(i in 1:10){
  size[i,1]<-1
}

#Making a copy of initimmune
finalimmune<-initimmune

#kk will give us total number of immune networks formed
kk<-10

#Taking various values of theta for the best results......
#Let---(theta=0.00)---

#Generation of final immune network
for(m in users){
  large<-0
  #  best<-1
  for(j in 1:10){
    dist<-0
    for(k in 1:size[j,1]){
      dist<-dist+cosine[m,finalimmune[j,k]] 
    }
    dist<-dist/size[j,1]
    if(identical((dist-large)>0,TRUE)==TRUE){
      large<-dist
      best<-j
    }
  }
  size[best,1]<-size[best,1]+1
  finalimmune[best,size[best,1]]<-users[m]
}

#Matrix to check which Train user is present in which network: networkof(943X1)
networkof<-matrix(nrow = Train_size,ncol = 1)
for(i in 1:10){
  for(j in 1:size[i,1]){
    networkof[finalimmune[i,j],1]<-i
  }
}