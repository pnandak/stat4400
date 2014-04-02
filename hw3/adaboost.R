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


#initialize loop conditions
b <- 1

#number of weak learners to train
B <- 4

#initilize weights, 1 per dim
weights <- t(as.matrix(rep(1/nrow(data),nrow(data)),nrow=1))

#initilize allPars which keeps B copies of (j, theta, m)
allPars <- list()

#these are the alphas
voting_weights <- list()


#Sample 4/5 of data as training set
index <- 1:ncol(data)
testindex <- sample(index, trunc(length(index)/5))
testset <- data[,testindex]
trainset <- data[,-testindex]

testclass <- matrix(unlist(class[,testindex]), nrow=1)
trainclass <- matrix(unlist(class[,-testindex]), nrow=1)

#number of vectors
count <- ncol(trainset)

while(b != B){
    #size = ncol(trainset)


    #split training data into 5 pieces
    #fit 5 different classifiers
    #Calculate validation error for each.
    #pick one with smallest validation error
    #print the error
    #print the test error (call agg_class)

    #fit classifier
    pars<-train(trainset, weights, trainclass)
    allPars <- rbind(allPars, pars)


    #compute classes
    classes <- classify(trainset, pars)


    #comput err
    sum_num<-0
    sum_den<-0
    for(i in 1:count){
        sum_num<-sum_num+weights[1,i]*(classes[1,i]!=trainclass[1,i])
        sum_den<-sum_den+weights[1,i]
    }   
    err<-sum_num/sum_den


    #compute alpha
    alpha<-log((1-err)/err)

    voting_weights <- rbind(voting_weights,alpha)


    #update weights
    for(i in 1:count){
        weights[1,i] <- weights[1,i]*exp(alpha * (classes[1,i] != trainclass[1,i]))
    }

    #loop
    b<-b+1
}
    
return(list(allPars=allPars, voting_weights=voting_weights))
}
