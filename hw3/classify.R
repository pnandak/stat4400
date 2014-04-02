#inputs
#X: matrix with columns as training vectors
#pars: list of parameters specifying the resulting classifier


#Outputs
#label: labels of data

classify <- function(X,pars){

labels<-list()
index<-pars[1,1] 

for(i in 1:ncol(X)){
    class<-(X[index,i]>pars[1,2])*pars[1,3]
    labels<-cbind(labels,class)
}


return(labels=labels)
}
