

boxplot(Default$balance~Default$student,col=c(5,20),xlab="Student Status",ylab= "Balance", main="Boxplot Balance versus Student Status")

boxplot(Default$income~Default$student,col=c(5,20),xlab="Student Status",ylab="Income",main="Boxplot Income versus Student Status")

summary(glm(default~balance, data=Default, family=binomial(link=logit)))
summary(glm(default~student, data=Default, family=binomial(link=logit)))
summary(glm(default~. ,data=Default, family=binomial(link=logit)))


x=(glm(Default,family=binomial(link=logit)))
xp=x$fitted.values
xp[xp>=0.5]=1
xp[xp<0.5]=0
table(Default$default,xp)
round(prop.table(table(Default$default,xp),1),2)

s1=sample(1:nrow(Default),nrow(Default)*0.7,replace=FALSE)
x=(glm(Default[s1,],family=binomial(link=logit)))
xp=x$fitted.values
xp[xp>=0.5]=1
xp[xp<0.5]=0
table(Default[s1,1],xp)
round(prop.table(table(Default[s1,1],xp),1),2)
xp=predict(x, newdata=Default[-s1,-1],type="response")
xp[xp>=0.5]=1
xp[xp<0.5]=0
table(Default[-s1,1],xp)
round(prop.table(table(Default[-s1,1],xp),1),2)


x=seq(-5,5,.02)
plot(x,dnorm(x),type="p", ylim=c(0,0.5), col=4,main="Similarity of Standard Normal and Logistic Densities", ylab ="Probability", xlab ="Quantiles")
points(x, 1.5*exp(-1.5*x)/(1+exp(-1.5*x))^2, col=5)

x=seq(-5,5,.02)
plot(x,pnorm(x),type="p",ylim=c(0,1),col=4,main="Standard Normal and Logistic Cumulative distributions",ylab="Cumulative Probability",xlab="Quantiles")
 points(x,1/(1+exp(-1.5*x))^2,col=5)


x=seq(-5,5,.02)
plot(x,1/(2*pi)^0.5 * exp(-0.5 * x^2),type="p", ylim=c(0,0.5), col=4,main="Similarity of Standard Normal and Gumbel densities", ylab ="Probability", xlab ="Quantiles")
points(x,1.1*exp(-1.1*x)*exp(-exp(-1.1*x)),col=11)

x=seq(-5,5,.02)
plot(x,pnorm(x),type="p",ylim=c(0,1),col=4,main="Standard Normal and Gumbel Cumulative distributions",ylab="Cumulative Probability",xlab="Quantiles")
points(x,exp(-exp(-1.1*x)),col=11)

#fashion=read.csv("c:\\....\\fashion.csv")
#Read fasion.cv file using the read.csv function into R.
#It can be downloaded from data sets folder in Course Material in Chalk.

fashion=read.csv(file="c:\\run\\fashion.csv")
#levels(fashion[,1])=levels(fashion[,1])[c(2,3,1)]
summary(fashion)
library(nnet)
x=multinom(relevel(RATING,ref="Low")~.,data=fashion)
summary(x)

table(fashion$RATING,predict(x,type="class"))
prop.table(table(fashion$RATING,predict(x,type="class")),2)
prop.table(table(fashion$RATING,predict(x,type="class")),1)

s1=sample(1:nrow(fashion),.7*nrow(fashion),replace=FALSE)
x=multinom(relevel(RATING,ref="Low")~.,data=fashion[s1,])
summary(x)

table(fashion$RATING[s1],predict(x,type="class"))
prop.table(table(fashion$RATING[s1],predict(x,type="class")),2)
prop.table(table(fashion$RATING[s1],predict(x,type="class")),1)

table(fashion$RATING[-s1],predict(x,newdata=fashion[-s1,], type="class"))
prop.table(table(fashion$RATING[-s1], predict(x, newdata=fashion[-s1,], type="class")),1)
prop.table(table(fashion$RATING[-s1], predict(x, newdata=fashion[-s1,], type="class")),2)

