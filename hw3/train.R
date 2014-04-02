#inputs
#X: matrix with columns as training vectors
#w: weights
#y: class labels

#Outputs
#pars: list of parameters specifying the resulting classifier

train <- function(X, w, y){

splitters <- list()
costs<- list()

#determine optimum thetas
for(i in 1:nrow(X)){
    
    #set default to first value
    split<-X[i,1]
    
    #calculate cost
    cost<-0
    for(l in 1:ncol(X)){
        cost<-cost+1*(1*(X[i,l]>split)!=y[1,l])
    }
    min<-cost/ncol(X)



    #for every other value
    for(j in 2:ncol(X)){
        cost<-0
        #if proportion of misclassified is less
        for(k in 1:ncol(X)){
            cost<-cost+1*(1*(X[i,k]>split)!=y[1,k])
        }
        min_new<-cost/ncol(X)
        if(min_new<=min){
            split<-X[i,k]
            min<-min_new
        }
    }

    #add to list of costs and splitters
    splitters<- cbind(splitters,split)
    costs <- cbind(costs,min)
}


#splitters is list of values to split on
#costs is list of costs


#determine optimum j 







#return tuple of (j,theta,m)



return(pars=pars)
}
