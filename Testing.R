#Testing Phase

TSD <- as(TestD,"realRatingMatrix")

#Computing the similarity between the Train and the Test user
cross_cosine <- similarity(TD,TSD,"cosine")

#Computing the group similarity of the test users
#group similarity matrix: groupsim
#kk is total number of immune networks

groupsim<-matrix(nrow=Test_size,ncol=kk)
for(i in 1:Test_size){
  for(j in 1:kk){
    gsim<-0
    count<-0
    for(k in 1:size[j,1]){
      if(identical(is.na(cross_cosine[finalimmune[j,k],i]),TRUE)==FALSE){
        gsim<-gsim+cross_cosine[finalimmune[j,k],i]
        count<-count+1
      }
    }
    gsim<-gsim/count
    groupsim[i,j]<-gsim
  }
}

#set of users useful for prediction for each Test user: predictset
#group similarity threshold= mean_gsim
#user similarity threshold= 0

predictset<-matrix(nrow=Test_size,ncol=Train_size)
for(pp in 1:Test_size){
  mm<-0
  for(jj in 1:10){
    if(identical((groupsim[pp,jj]-mean(groupsim[pp,]))>0,TRUE)==TRUE){
      for(q in 1:size[jj,1]){
        if(identical((cross_cosine[finalimmune[jj,q],pp]-0.00)>0,TRUE)==TRUE){
          mm<-mm+1
          predictset[pp,mm]<-finalimmune[jj,q]
        }
      }
    }
  }
}