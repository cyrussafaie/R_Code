source("clustreg.txt")
source("clustreg.predict.txt")
fashion2=read.table("fashion2.txt")
library(rpart)
library(flexmix)


x1=runif(100,-1.1)
y1=0.6*x1+rnorm(100,0,0.05)
x2=runif(100,-1,1)
y2=-0.6*x2+rnorm(100,0,0.05)
plot(x1,y1,ylim=c(-1.5,1.5),xlim=c(-1.5,1.5),col=4,main="Plots of two lines in opp directions",ylab="Values of y1 and y2", xlab= "values of x1 and x2")
points(x2,y2,pch="*",col=6)


summary(lm(y1~x1))

summary(lm(y2~x2))

summary(lm(c(y1,y2)~c(x1,x2)))


yhat=predict(lm(c(y1,y2)~c(x1,x2)))
plot(x1,y1,ylim=c(-1.5,1.5),xlim=c(-1.5,1.5),col=4,main="Plots of two lines in opp directions",ylab="Values of y1 and y2",xlab="values of x1 and x2")
points(x2,y2,pch="*",col=6)
points(c(x1,x2),yhat,col=7)


x=rpart(car.test.frame[,c(1,4,6,7,8)],cp=0)
predict(x)
cor(car.test.frame[,1],predict(x))^2 

par(mai=c(0.1,0.1,0.1,0.1))
plot(x,main="Regression Tree: car.test.frame",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(x,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))


clustreg.cars.1=clustreg(car.test.frame[,c(1,4,6,7,8)],1,1,1121,1)
clustreg.cars.2=clustreg(car.test.frame[,c(1,4,6,7,8)],2,1,1121,10)
clustreg.cars.3=clustreg(car.test.frame[,c(1,4,6,7,8)],3,24,1121,10)

plot(c(1,2,3),c(clustreg.cars.1$rsq.best,clustreg.cars.2$rsq.best,clustreg.cars.3$rsq.best),ylim=c(0,1),type="l",col=4,main="VAF Plot for Cars Data: Cluster-wise Regression", ylab="Variance Accounted For",xlab="Number of Clusters")  


clustreg.cars.1=clustreg(car.test.frame[,c(1,4,6,7,8)],1,1,1121,1)
clustreg.cars.1$results
clustreg.cars.2= clustreg(car.test.frame[,c(1,4,6,7,8)],2,1,1121,10)
clustreg.cars.2$results[[1]]
clustreg.cars.2$results[[2]]
car.test.frame[clustreg.cars.2$cluster==2,c(1,4,6:8)]

summary(fashion2)
x=rpart(fashion2)
cor(predict(rpart(fashion2)),fashion2[,1])^2

par(mai=c(0.1,0.1,0.1,0.1))
plot(x,main="Regression Tree: Fashion2 Data",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(x,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))


clustreg.fashion2.1=clustreg(fashion2, 1,1,1121,1)
clustreg.fashion2.2=clustreg(fashion2,2,25,1121,12)
clustreg.fashion2.3=clustreg(fashion2,3,25,1121,12)
clustreg.fashion2.1$summary
clustreg.fashion2.2$summary
#plot(c(1,2,3),c(clustreg.fashion2.1$rsq.best,clustreg.fashion2.2$rsq.best,clustreg.fashion2.3$rsq.best),ylim=c(0,1),type="l",col=4,main="VAF Plot for Fashion2 Data: Cluster-wise Regression", ylab="Variance Accounted For",xlab="Number of Clusters")  
table(fashion2[clustreg.fashion2.2$cluster ==2,1], fashion2[clustreg.fashion2.2$cluster==2,4])
table(fashion2[,1], fashion2[,4])      


samp1=sample(1:2000,1400,replace=FALSE)
clustreg.fashion2.samp1.1=clustreg(fashion2[samp1,],1,1,1213,1)
clustreg.fashion2.samp1.2=clustreg(fashion2[samp1,],2,20,1213,15)


clustreg.fashion2.samp1.1$results
clustreg.fashion2.samp1.2$results[[1]]
clustreg.fashion2.samp1.2$results[[2]]
ho=clustreg.predict(clustreg.fashion2.samp1.2, fashion2[-samp1,])

ho$rsq
table((ho$cluster))
round(prop.table(table((ho$cluster))),3)





library(flexmix)
data(car.test.frame,package="rpart")
LCMix.cars.1=initFlexmix(Price~Mileage+Weight+Disp.+HP, data=car.test.frame, k=1, model = FLXMRglm(family = "gaussian"),nrep=100)

LCMix.cars.2=initFlexmix(Price~Mileage+Weight+Disp.+HP, data=car.test.frame, k=2, model = FLXMRglm(family = "gaussian"),nrep=100)

LCMix.cars.3=initFlexmix(Price~Mileage+Weight+Disp.+HP, data=car.test.frame, k=3, model = FLXMRglm(family = "gaussian"),nrep=100)

LCMix.cars.123=initFlexmix(Price~Mileage+Weight+Disp.+HP, data=car.test.frame, k=1:3, model = FLXMRglm(family = "gaussian"),nrep=100)

unique(LCMix.cars.123)
plot(LCMix.cars.2)
attributes(LCMix.cars.2)$size
table(attributes(LCMix.cars.2)$cluster) 
prop.table(table(attributes(LCMix.cars.2)$cluster))
attributes(LCMix.cars.2)$components

predict(LCMix.cars.2)
p=rep(NA,60)
p1=predict(LCMix.cars.2)$Comp.1
p2=predict(LCMix.cars.2)$Comp.2
p[clusters(LCMix.cars.2)==2]=p2[clusters(LCMix.cars.2)==2]
p[clusters(LCMix.cars.2)==1]=p1[clusters(LCMix.cars.2)==1]
cor(p,car.test.frame[,1])^2





