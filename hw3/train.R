#inputs
#X: matrix with columns as training vectors
#w: weights
#y: class labels

#Outputs
#pars: list of parameters specifying the resulting classifier

train <- function(X, w, y){

splitters <- list()
split_costs<- list()

#----------------------
#determine optimum thetas
#----------------------
for(i in 1:nrow(X)){
    
    #set default to first value
    split<-X[i,1]
    
    #calculate cost
    cost<-0
    for(l in 1:ncol(X)){
        cost<-cost+1*(1*(X[i,l]>split)!=y[1,l])
    }
    min_theta<-cost/ncol(X)

    #for every other value
    for(j in 2:ncol(X)){
        cost<-0
        #if proportion of misclassified is less
        for(k in 1:ncol(X)){
            cost<-cost+1*(1*(X[i,k]>split)!=y[1,k])
        }
        min__theta_new<-cost/ncol(X)
        if(min__theta_new<=min_theta){
            split<-X[i,k]
            min_theta<-min_theta_new
        }
    }

    #add to list of costs and splitters
    splitters<- cbind(splitters,split)
    split_costs <- cbind(split_costs,min_theta)
}

#splitters is list of values to split on
#costs is list of costs
#----------------------




#-----------------
#determine optimum j 
#-----------------
min_j <- 1
sum_num<-0
sum_den<-0
#calculate cost 
for(i in 1:ncol(X)){
    sum_num<-sum_num+ w[1,i]*(y[1,i]!=(X[1,i]>splitters[1,1]) )
    sum_den<-sum_den+w[1,i]
}
min_j_weight<-sum_num/sum_den


for(j in 2:nrow(X)){
    sum_num<-0
    sum_den<-0
    #calculate cost 
    for(i in 1:ncol(X)){
        sum_num<-sum_num+ w[1,i]*(y[1,i]!=(X[j,i]>splitters[1,j]) )
        sum_den<-sum_den+w[1,i]
    }
    min_j_weight_new<-sum_num/sum_den
    if(min_j_weight_new<=min_j_weight){
        min_j<-j
        min_j_weight<-min_j_weight_new
    }
}

#-----------------



#return tuple of (j,theta,m)
pars<-rbind(min_j,splitters[1,j],1)


return(pars=pars)
}
