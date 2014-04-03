#inputs
#X: matrix with columns as training vectors
#pars: list of parameters specifying the resulting classifier


#Outputs
#label: labels of data

classify <- function(X,pars){
labels<-list()
index<-as.integer(pars[1,1]) 


for(i in 1:ncol(X)){
    m<-as.double(pars[1,3])
    class<-m*(as.double(X[index,i])>as.double(pars[1,2]))
    if(class==0){
        class<- -1*m
    }
    labels<-cbind(labels,class)
}

return(labels=labels)
}
