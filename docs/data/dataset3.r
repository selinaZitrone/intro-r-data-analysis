#Dataset 3 - Britta
#Riverine rabbit (count data of droppings) dependent on
#distance to nearest river (metric)
#degradation (factor: "low", "medium", "high")

nattempts <- 187
distance <- round(runif(n=nattempts,min=5,max=1000),0)
temp <- runif(n=nattempts,min=0,max=1)
degradation <- as.factor(ifelse(temp<0.3,"low",
                            ifelse(temp<0.7,"medium","veryhigh")))


sights <- array(data=0,dim=nattempts)
for(i in 1:nattempts){
    if(degradation[i]=="low"){
      sights[i] <- round((1000-distance[i])/50 + rnorm(n=1,mean=0,sd=(1000-distance[i])/100),0)
    } else if (degradation[i]=="medium"){
      sights[i] <- round((1000-distance[i])/80 + rnorm(n=1,mean=0,sd=(1000-distance[i])/100),0)   
    }
    else {
      sights[i] <- round((1000-distance[i])/80 + rnorm(n=1,mean=0,sd=(1000-distance[i])/100),0)
    }
}
sights[sights<0] <- 0



mycol <- c("red","blue","darkgreen")
mypch <- c(15,20,17)
plot(x=distance,y=sights,col=mycol[degradation],pch=mypch[degradation])


test <- lm(sights~distance*degradation)
summary(test)
par(mfrow=c(2,2))
plot(test)
par(mfrow=c(1,1))

dataset3<-as.data.frame(cbind(distance,degradation,sights))
names(dataset3) <-c("distance","degradation","sights")
dataset3$degradation[dataset3$degradation==1] <- "low"
dataset3$degradation[dataset3$degradation==2] <- "medium"
dataset3$degradation[dataset3$degradation==3] <- "veryhigh"

write.table(dataset3,"dataset3.txt",quote=F,row.names = F)

rm(list = ls())
#-----------------------------------------------------------------------------
#START
#-----------------------------------------------------------------------------

#glm with poiss