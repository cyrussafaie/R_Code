kmodes=function (data = data, nclust = nclust, niterations = niterations, nloops = nloops, seed = seed) 
{
    prevMAF = -1
    niterations = 25
    set.seed(seed)
    for (i in 1:nloops) {
        z = fun.kmodes(data = data, nclust = nclust,niterations=niterations)
        if (z$MAF > prevMAF) {
            prevMAF = z$MAF
            ind = i
            z.old = z
        }
    }
    return(list(data = z.old$Data, 
        Group = z.old$Groups, Centroids = z.old$Centroids,  
	  Cluster.Sizes= z.old$Cluster.Sizes,
        MAF = z.old$MAF, iteration = ind, 
        seed = seed))
}

fun.kmodes=function (data = data, nclust = nclust,niterations=niterations) 
{
    data=as.data.frame(data)
    nam=names(data)
    data=apply(data,2,factor)
    M = nrow(data)
    N = ncol(data)
    K = nclust
    S = sample(1:K,M,replace=TRUE)
    W = matrix("NA", K, N)
    datahat=matrix("NA",M,N)
    i = 1
    while ((i <= niterations)) {
        		for(j in 1:N) {
				W[,j]=tapply(data[,j],S,fun.mod)
        		}
        
                 hst= 0
 #               print(W)
        		for(j in 1:M) {
				tmp=rep(0,K)
				for (k in 1:K){

					ttt = (data[j,])==(W[k,])
					tmp[k]= length(ttt[ttt==TRUE])		
					
						}	
                    l = seq(1:K)[tmp==max(tmp)]
				    if(length(l) == 1) S[j]=l 
                    if(length(l) > 1) S[j] = sample(l,1)
					datahat[j,] = W[S[j],]
					hst=hst+max(tmp)
					}	
#                print(c(i, hst))
        
#			for(j in 1:M) {
#				for(n in 1:N) {
#				if(!is.na(data[j,n]) && (datahat[j,n] == data[j,n])) hst[i] = hst[i]+1
        
			i=i+1
    }
		W=data.frame(W)
		names(W) = nam
		W = W[sort(unique(S)),]
		if(nrow(W) >1) {row.names(W) = sort(unique(S))}    
     rrr = list(Groups = S, Cluster.Sizes = table(S), Centroids = W, MAF = hst/(M*N))


    return(rrr)
}

fun.mod=function(x){

y=factor(x)
z=table(y)
zz=z[z==max(z)]
n=names(zz)
if(length(n) > 1) n=sample(n,1)
return(n)


}

