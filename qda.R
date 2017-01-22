fashion=read.table("fashion.txt")
library(MASS)
library(rpart)



x=matrix(c(rep(1,5),rep(0,5),2:11,c(4,2,5,4,7,6,4,7,6,9)),nrow=10,ncol=3)
x=data.frame(x)
names(x) = c("Y","X1","X2")
plot(x[,2],x[,3],col=x[,1]+3,pch="o",xlab="X1",ylab="X2")
plot(x[,2]-mean(x[,2]),x[,3]-mean(x[,3]),col=x[,1]+3,pch="o",xlab="X1",ylab="X2")
z=lda(Y ~ X1 + X2, data = x)
print(z)
z$scaling/(sum(z$scaling^2))^0.5
plot(z)
predict(z)


summary(lm(x))
z=lm(x)
z$coefficients[2]/(sum(z$coefficients[2]^2+z$coefficients[3]^2)^0.5)
z$coefficients[3]/(sum(z$coefficients[2]^2+z$coefficients[3]^2)^0.5)


summary(fashion)
z=lda(RATING~., data=fashion,CV=FALSE)
z$svd^2/sum(z$svd^2)
round(t(t(z$scaling)/(apply(z$scaling^2,2,sum))^0.5),2)
z$prior
round(z$means,2)

table(fashion[,1],predict(z)$class)
round(prop.table(table(fashion[,1],predict(z)$class),1),2)
table(fashion[,1],predict(rpart(fashion,cp=0),type="class"))
round(prop.table(table(fashion[,1],predict(rpart(fashion,cp=0),type="class")),1),2)




z=qda(RATING~., data=fashion,CV=FALSE)
z$prior
round(z$means,2)
table(fashion[,1],predict(z)$class)
round(prop.table(table(fashion[,1],predict(z)$class),1),2)
