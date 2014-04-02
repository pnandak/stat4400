#inputs
#data: training data
#class: training data classes

#Outputs
#label: labels of data

adaboost <- function(data, class){

#read in data
d <- read.table(data, header = FALSE, sep = "", skip = 0)
c <- read.table(class, header = FALSE, skip = 0)
data <- t(as.matrix(d))
class <- matrix(unlist(c), nrow=1)



#Data is BxM matrix, where there are M
#Vectors each with B dimensions

#Class is 1xM matrix where there are
#M values for the class of each of the M vectors


#initialize loop conditions
b <- 1
B <- nrow(data)

#number of vectors
count <- ncol(data)

#initilize weights, 1 per dim
weights <- t(as.matrix(rep(1/B,B),nrow=1))

#initilize allPars which keeps B copies of (j, theta, m)
allPars <- list()

#these are the alphas
voting_weights <- list()

while(b != B){
    #fit classifier
    pars<-train(data, weights, class)
    allPars <- cbind(allPars, pars)


    #compute classes
    classes <- classify(data, pars)


    #comput err
    sum_num<-0
    sum_den<-0
    for(i in 1:count){
        sum_num<-sum_num+weights[1,i]*(classes[1,i]!=class[1,i])
        sum_den<-sum_den+weights[1,i]
    }   
    err<-sum_num/sum_den


    #compute alpha
    alpha<-log((1-err)/err)
    voting_weights <- cbind(voting_weights,alpha)


    #update weights
    for(i in 1:count){
        weights[1,i] <- weights[1,i]*exp(alpha * (classes[1,i] != class[1,i]))
    }

    #loop
    b<-b+1
}
    
return(allPars=allPars, voting_weights=voting_weights)
}
