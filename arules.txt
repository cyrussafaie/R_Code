


require(arules)
data(Groceries)
dim(Groceries)
head(Groceries)
inspect(head(Groceries))
#####inspect(Groceries) # TOO LONG
summary(Groceries)
round(itemFrequency(Groceries)[1:30],4)
table(apply(as(Groceries,"matrix"),1,sum))
sum(table(apply(as(Groceries,"matrix"),1,sum)))
itemFrequencyPlot(Groceries, support = 0.15, cex.names=0.8)
itemFrequencyPlot(Groceries, support = 0.10, cex.names=0.8)
#####itemFrequencyPlot(Groceries[1:100], support = 0.1, cex.names=0.8)
eclatres=eclat(Groceries,parameter=list(support=0.001))
inspect(head(eclatres))
inspect(head(sort(eclatres)))
summary(eclatres)
inspect(subset(sort(eclatres), subset = support > .06))
eclatresm=as(items(eclatres),"matrix")
dim(eclatresm)
image(eclatresm,col=4:6)

apriorires=apriori(Groceries, parameter=list(support=.001,confidence=0.5,ext=TRUE))
inspect(head(apriorires))
inspect(head(sort(apriorires)))
summary(apriorires)
inspect(subset(sort(apriorires), lift>6 & confidence > 0.9))


