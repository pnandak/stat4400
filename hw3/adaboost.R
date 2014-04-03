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
class <- matrix(unlist(c) , nrow=1)

index <- 1:ncol(data)
testindex<- sample(index,trunc(length(index)/5))
testset <- data[,testindex]
trainset <- data[,-testindex]

testclass <- matrix(unlist(class[,testindex]),nrow=1)
trainclass <- matrix(unlist(class[,-testindex]),nrow=1)


#initialize loop conditions
b <- 1

#number of weak learners to train
B <- 51

#initilize allPars which keeps B copies of (j, theta, m)
allPars <- list()

#these are the alphas
voting_weights <- list()

#outputs
test_errors <- list()
train_errors <-list()

#initilize weights, 1 per vec
weights <- t(as.matrix(rep(1/ncol(trainset),ncol(trainset)),nrow=1))


while(b != B){
    print(paste("Beginning iteration ", toString(b)))
    #split training data into 5 sets
    one<-ncol(trainset)/5
    two<-2*ncol(trainset)/5
    three<-3*ncol(trainset)/5
    four<-4*ncol(trainset)/5
    five<-ncol(trainset)

    #validation contains 1/5 of data
    validset1<-trainset[,1:one]
    validset2<-trainset[,one:two]
    validset3<-trainset[,two:three]
    validset4<-trainset[,three:four]
    validset5<-trainset[,four:five]
    validclass1<-t(as.matrix(trainclass[,1:one]))
    validclass2<-t(as.matrix(trainclass[,one:two]))
    validclass3<-t(as.matrix(trainclass[,two:three]))
    validclass4<-t(as.matrix(trainclass[,three:four]))
    validclass5<-t(as.matrix(trainclass[,four:five]))
    weightsvalid1<-t(as.matrix(weights[,1:one]))
    weightsvalid2<-t(as.matrix(weights[,one:two]))
    weightsvalid3<-t(as.matrix(weights[,two:three]))
    weightsvalid4<-t(as.matrix(weights[,three:four]))
    weightsvalid5<-t(as.matrix(weights[,four:five]))

    #training contains 4/5 of data
    trainset1<-cbind(validset2,validset3, validset4,validset5)
    trainset2<-cbind(validset1,validset3, validset4,validset5)
    trainset3<-cbind(validset1,validset2, validset4,validset5)
    trainset4<-cbind(validset1,validset2, validset3,validset5)
    trainset5<-cbind(validset1,validset2, validset3,validset4)
    trainclass1<-t(as.matrix(c(validclass2,validclass3, validclass4,validclass5)))
    trainclass2<-t(as.matrix(c(validclass1,validclass3, validclass4,validclass5)))
    trainclass3<-t(as.matrix(c(validclass1,validclass2, validclass4,validclass5)))
    trainclass4<-t(as.matrix(c(validclass1,validclass2, validclass3,validclass5)))
    trainclass5<-t(as.matrix(c(validclass1,validclass2, validclass3,validclass4)))
    weightstrain1<-t(as.matrix(c(weightsvalid2,weightsvalid3,weightsvalid4,weightsvalid5)))
    weightstrain2<-t(as.matrix(c(weightsvalid1,weightsvalid3,weightsvalid4,weightsvalid5)))
    weightstrain3<-t(as.matrix(c(weightsvalid1,weightsvalid2,weightsvalid4,weightsvalid5)))
    weightstrain4<-t(as.matrix(c(weightsvalid1,weightsvalid2,weightsvalid3,weightsvalid5)))
    weightstrain5<-t(as.matrix(c(weightsvalid1,weightsvalid2,weightsvalid3,weightsvalid4)))


    #fit classifiers
    pars1<-train(trainset1, weightstrain1, trainclass1)
    pars2<-train(trainset2, weightstrain2, trainclass2)
    pars3<-train(trainset3, weightstrain3, trainclass3)
    pars4<-train(trainset4, weightstrain4, trainclass4)
    pars5<-train(trainset5, weightstrain5, trainclass5)

    #compute validation classes
    classes1 <- classify(validset1, pars1)
    classes2 <- classify(validset2, pars2)
    classes3 <- classify(validset3, pars3)
    classes4 <- classify(validset4, pars4)
    classes5 <- classify(validset5, pars5)

    #comput err1
    sum_num1<-0
    sum_den1<-0
    for(i in 1:ncol(classes1)){
        sum_num1<-sum_num1+weightsvalid1[1,i]*(classes1[1,i]!=validclass1[1,i])
        sum_den1<-sum_den1+weightsvalid1[1,i]
    }   
    err1<-sum_num1/sum_den1

    #comput err2
    sum_num2<-0
    sum_den2<-0
    for(i in 1:ncol(classes2)){
        sum_num2<-sum_num2+weightsvalid2[1,i]*(classes2[1,i]!=validclass2[1,i])
        sum_den2<-sum_den2+weightsvalid2[1,i]
    }   
    err2<-sum_num2/sum_den2
  
    #comput err3
    sum_num3<-0
    sum_den3<-0
    for(i in 1:ncol(classes3)){
        sum_num3<-sum_num3+weightsvalid3[1,i]*(classes3[1,i]!=validclass3[1,i])
        sum_den3<-sum_den3+weightsvalid3[1,i]
    }   
    err3<-sum_num3/sum_den3
  
    #comput err4
    sum_num4<-0
    sum_den4<-0
    for(i in 1:ncol(classes4)){
        sum_num4<-sum_num4+weightsvalid4[1,i]*(classes4[1,i]!=validclass4[1,i])
        sum_den4<-sum_den4+weightsvalid4[1,i]
    }   
    err4<-sum_num4/sum_den4

    #comput err5
    sum_num5<-0
    sum_den5<-0
    for(i in 1:ncol(classes5)){
        sum_num5<-sum_num5+weightsvalid5[1,i]*(classes5[1,i]!=validclass5[1,i])
        sum_den5<-sum_den5+weightsvalid5[1,i]
    }   
    err5<-sum_num5/sum_den5
  
 
    #select minimum of the errors and use as parameters
    errors<- c(err1,err2,err3,err4,err5)
    min <-which.min(errors)
    if(min==1){
        pars<- pars1
    }
    else if(min ==2){
        pars<-pars2
    }
    else if(min==3){
        pars<-pars3
    }
    else if(min==4){
        pars<-pars4
    }
    else{
        pars<-pars5
    }



    classes <- classify(trainset, pars)
    allPars <- rbind(allPars, pars)
 
    #comput err
    sum_num<-0
    sum_den<-0
    for(i in 1:ncol(trainset)){
        sum_num<-sum_num+weights[1,i]*(classes[1,i]!=trainclass[1,i])
        sum_den<-sum_den+weights[1,i]
    }   
    err<-sum_num/sum_den
    print(paste("Error: ", toString(err)))
  
    #compute alpha
    alpha<-log((1-err)/err)
    voting_weights <- rbind(voting_weights,alpha)


    #update weights
    for(i in 1:ncol(trainset)){
        weights[1,i] <- weights[1,i]*exp(alpha * (classes[1,i] != trainclass[1,i]))
    }



    #comput validation error
    c_hat<-agg_class(trainset,voting_weights,allPars)
    err_num<-0
    for(i in 1:ncol(trainclass)){
        if(trainclass[1,i]!=c_hat[1,i]){
            err_num<-err_num+1
        }
    }
    train_errors<-cbind(train_errors, err_num/ncol(trainclass))
    print(paste("Misclassification rate (validation):", toString(err_num/ncol(trainclass))))




    #output misclassification rate on test data so far
    c_hat<-agg_class(testset,voting_weights,allPars)
    err_num<-0
    for(i in 1:ncol(testclass)){
        if(testclass[1,i]!=c_hat[1,i]){
            err_num<-err_num+1
        }
    }
    test_errors<-cbind(test_errors, err_num/ncol(testclass))
    print(paste("Misclassification rate (test):", toString(err_num/ncol(testclass))))

    #loop
    b<-b+1
}
    


return(test_errors=test_errors)
}
