require(rpart)

x=runif(100)
plot(x, -x*log(x) - (1-x)*log(1-x),col=4, main="Entropy and Gini Index",xlab="P",ylab="Impurity Measure")
points(x,1-x^2-(1-x)^2,col=6)
points(x,1-apply(cbind(x,1-x),1,max),col=3)

x=runif(100)
plot(x, -x*log(x) - (1-x)*log(1-x),col=4, main="Entropy and Gini Index",xlab="P",ylab="Impurity Measure")
points(x,1-x^2-(1-x)^2,col=6)
points(x,1-apply(cbind(x,1-x),1,max),col=3)
points(x,1+(log(x)+log(1-x))/max(abs(log(x)+log(1-x))),col=5) 

summary(fashion)
x=rpart(fashion,control=rpart.control(cp=0,minsplit=10,xval=10, maxsurrogate=0))
par(mai=c(0.1,0.1,0.1,0.1))
plot(x,main="Classification Tree: Fashion Data",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(x,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))

table(fashion$RATING)
table(fashion$RATING,fashion$FASHION)
prop.table(table(fashion$RATING,fashion$FASHION),2)

x=rpart(fashion,control=rpart.control(cp=0,minsplit=10,xval=10, maxsurrogate=0))
printcp(x)

x=rpart(fashion,control=rpart.control(cp=0,minsplit=10,xval=10, maxsurrogate=0))
plotcp(x,minline=TRUE,col=4)


table(fashion[,1],predict(x,type="class"))
round(prop.table(table(fashion[,1],predict(x,type="class")),1),2)        
round(prop.table(table(fashion[,1],predict(x,type="class")),2),2)

s1=sample(1:2000,1400,replace=FALSE)
x=rpart(fashion[s1,],control=rpart.control(cp=0,minsplit=10,xval=10, maxsurrogate=0))
par(mai=c(0.1,0.1,0.1,0.1))
plot(x,main="Classification Tree: Fashion Data",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(x,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))
plotcp(x,minline=TRUE,col=4)
printcp(x)



z=rpart(fashion[s1,],control=rpart.control(cp=0.0003962))
par(mai=c(0.1,0.1,0.1,0.1))
plot(z,main="Classification Tree: Fashion Data",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(z,cex=0.6,col=4,use.n=TRUE,fancy=TRUE, fwidth=0.4, fheight=0.4,bg=c(5))

table(fashion[s1,1],predict(z,type="class"))
round(prop.table(table(fashion[s1,1],predict(z,type="class")),1),2)        
round(prop.table(table(fashion[s1,1],predict(z,type="class")),2),2)
table(fashion[-s1,1],predict(z,newdata=fashion[-s1,-1],type="class"))
round(prop.table(table(fashion[-s1,1],predict(z,newdata=fashion[-s1,-1],type="class")),1),2)
round(prop.table(table(fashion[-s1,1],predict(z,newdata=fashion[-s1,-1],type="class")),2),2)



D=runif(100)*10
s=sample(1:100,70,replace=FALSE)
G1=D[s]
G2=D[-s] 
TSS = sum((D-mean(D))^2)
BGSS=70*(mean(G1)-mean(D))^2 + 30*(mean(G2)-mean(D))^2
WGSS=sum((G1-mean(G1))^2)+sum((G2-mean(G2))^2)
BGSS+WGSS
TSS
rm(TSS,BGSS,WGSS,D,s,G1,G2) 

library(rpart)
data(car.test.frame)
x=rpart(car.test.frame,control=rpart.control(method="anova"))
cor(car.test.frame[,1],predict(rpart(car.test.frame,control=rpart.control(method="anova"))))^2
par(mai=c(0.1,0.1,0.1,0.1))
plot(x,main="Regression Tree: car.test.frame",col=3, compress=TRUE, branch=0.2,uniform=TRUE)
text(x,cex=0.6,col=4,use.n=TRUE,fancy=TRUE,fwidth=0.4,fheight=0.4,bg=c(5))

