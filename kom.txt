illustration.data=matrix(c(
0,10,4,7,12,
10,0,8,6,11,
4,8,0,10,3,
7,6,10,0,9,
12,11,3,9,0),
5,5)
par(mfrow=c(2,2))
plot(hclust(as.dist(illustration.data), "single"),col=4)
plot(hclust(as.dist(illustration.data), "complete"),col=4)
plot(hclust(as.dist(illustration.data), "average"),col=4)
plot(hclust(as.dist(illustration.data), "centroid"),col=4)
cutree(hclust(as.dist(illustration.data), "complete"),2)


#read the protein data set  from file using read.csv command.
names(protein)
protein.2.3=kmeans(protein[,2:3],centers=3,nstart=50)
o=order(protein.2.3$cluster)
data.frame(protein[o,1],protein.2.3$cluster[o])
plot(protein[,2],protein[,3],type="n",xlab="Red Meat",ylab="White Meat")
text(protein[,2],protein[,3],labels=protein[,1],col=protein.2.3$cluster+1)

#Scaled data
protein.2.3=kmeans(scale(protein[,2:3]),centers=3,nstart=50)
o=order(protein.2.3$cluster)
data.frame(protein[o,1],protein.2.3$cluster[o])
plot(protein[,2],protein[,3],type="n",xlab="Red Meet",ylab="White Meet")
text(protein[,2],protein[,3],labels=protein[,1],col=protein.2.3$cluster+1)

#komeans
names(protein)
z=komeans(protein[,2:3],nclust=3,lnorm=2,tolerance=.001,nloops = 120,seed=3) 
plot(protein[,2],protein[,3],type="n",xlab="Red Meet",ylab="White Meet")
text(protein[,2],protein[,3],labels=protein[,1],col=z$Group+1)

protein.okc.3.L1=komeans(protein[,-1], nclust=3, lnorm=1, tolerance=.001, nloops = 260, seed= 975647871) 
plot(protein[,2],protein[,3],type="n",xlab="Red Meet",ylab="White Meet")
text(protein[,2],protein[,3],labels=protein[,1],col=protein.okc.3.L1$Group+1)

