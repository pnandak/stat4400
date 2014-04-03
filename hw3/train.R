#inputs
#X: matrix with columns as training vectors
#w: weights
#y: class labels

#Outputs
#pars: list of parameters specifying the resulting classifier

train <- function(X, w, y){

splitters <- list()
split_costs<- list()
split_m <- list()



#----------------------
#determine optimum thetas
#----------------------
print("Determining optimum thetas (this may take a moment)")
for(i in 1:nrow(X)){
    #if((i%%20)==0)
     #   print(paste("Calculating theta:", toString(ncol(splitters) )))

    #set default to first value
    split<-X[i,1]
    m<-y[1,1]
    
    #calculate cost
    sum_num<-0
    sum_den<-0
    for(l in 1:ncol(X)){
        class<-(m*(X[i,l]>split))
        if(class==0){
            class<- -1*m
        }
        sum_num<-sum_num+w[1,l]*(class!=y[1,l])
        sum_den<-sum_den+w[1,l]
    }
    
    min_theta<-sum_num/sum_den
    min_m<-m

    #if this is the correct split point
    if(min_theta==0){
        #add to list of costs and splitters
        splitters<- cbind(splitters,split)
        split_costs <- cbind(split_costs,min_theta)
        split_m<-cbind(split_m,min_m)
        next
    }

    #for every other value
    for(j in 2:ncol(X)){
        sum_num<-0
        sum_den<-0
        m<-y[1,j]
        split_new<-X[i,j]

        #if proportion of misclassified is less
        for(k in 1:ncol(X)){
            class<-(m*(X[i,k]>split_new))
            if(class==0){
                class<- -1*m
            }
            sum_num<-sum_num+w[1,k]*(class!=y[1,k])
            sum_den<-sum_den+w[1,k]
        }
        min_theta_new<-sum_num/sum_den

        if(min_theta_new<=min_theta){
            split<-split_new
            min_theta<-min_theta_new
            min_m<-m
        }
        if(min_theta_new==0){
            break
        }
    }


    #add to list of costs and splitters
    splitters<- cbind(splitters,split)
    split_costs <- cbind(split_costs,min_theta)
    split_m<-cbind(split_m,min_m)


    if(min_theta>.5){
        print("error.")
    }
}

#splitters is list of values to split on
#costs is list of costs
#----------------------








#-----------------
#determine optimum j 
#-----------------
print("Determining minimum j")
min_j <- 1
sum_num<-0
sum_den<-0
#calculate cost 
for(i in 1:ncol(X)){
    class<- as.integer(split_m[1,1])*1*(X[1,i]>splitters[1,1])
    if(class==0){
        class<- -1*as.integer(split_m[1,1])
    }
    sum_num<-sum_num+ w[1,i]*(y[1,i]!=class)
    sum_den<-sum_den+w[1,i]
}
min_j_weight<-sum_num/sum_den


for(j in 2:nrow(X)){
    sum_num<-0
    sum_den<-0
    #calculate cost 
    for(i in 1:ncol(X)){
        class<- as.integer(split_m[1,j])*1*(X[j,i]>splitters[1,j])
        if(class==0){
            class<- -1*as.integer(split_m[1,j])
        }
        sum_num <- sum_num + w[1,i] * (y[1,i]!= class)
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
pars<-cbind(min_j,splitters[1,min_j],split_m[1,min_j])
pars <- as.matrix(pars, nrow=1)
print(pars)

return(pars=pars)
}

