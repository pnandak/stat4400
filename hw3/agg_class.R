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
            index<-as.integer(all_pars[j,1])
            m<-as.integer(all_pars[j,3])
            theta<-as.integer(all_pars[j,2])
            class<-(m*(X[index,i]>theta))
            if(class==0){
                class<- -1*m
            }
            sum<-sum+as.double(alpha[j,1])*class
        }
        classes<-cbind(classes,sign(sum))
    }


return(classes=classes)
}
