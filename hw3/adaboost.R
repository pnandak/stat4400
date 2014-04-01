#inputs
#data: training data
#class: training data classes

#Outputs
#label: labels of data

adaboost <- function(data, class){

#read in data
d <- read.table(data, header = FALSE, sep = "", skip = 0)
c <- read.table(class, header = FALSE, skip = 0)
data <- as.matrix(d)
class <- as.matrix(c)

#initialize loop conditions
b <- 1
B <- nrow(data)

count <- ncol(data)
#initilize weights
weights <- as.matrix(rep(1/B,B),nrow=1)

#initilize allPars which keeps B copies of (j, theta, m)
allPars <- list()

while(b != B){
    #fit classifier
    pars<-train(data, weights, class, b)
    allPars <- cbind(allPars, pars)

    classes <- classify(data, pars, b)

    #comput err


    #compute alpha


    #update weights


    #loop
    b<-b+1
}
    
return(allPars=allPars)
}
