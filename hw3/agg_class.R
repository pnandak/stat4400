#inputs
#X: matrix with columns as test vectors
#alpha: vector of voting weights
#all_pars: parameters of weak learners

#Outputs
#

agg_class <- function(X, alpha, all_pars){
    classes<-list()

    for(i in 1:ncol(X)){
        sum<-0
        for(j in 1:nrow(alpha)){
            class<-(all_pars[j,3]*(X[all_pars[j,1],i]>=all_pars[j,2]))
            if(class==0){
                class<- -1*all_pars[j,3]
            }
            sum<-sum+alpha[j,1]*class
        }
        classes<-cbind(classes,sign(sum))

    }
return(classes=classes)
}
