#inputs
#S: set of vectors to be trained around
#y: class labels

#Outputs
#z: hyperplane defining classifier
#Z_history: history of transformation of Z over iterations

perceptrain <- function(S){
    y <- do.call(rbind, S[2])
    S <- do.call(rbind, S[1])

    num <- nrow(S)
    dim <- ncol(S)

    #current hyperplane
    Z <- as.matrix(rep(1, dim), ncol=1)
 
    #history list
    Z_history <- list()
    Z_history <- cbind(Z_history,Z)


    #iteration
    k <- 1 


    while(abs(cost(S,y,Z,dim,num)) != 0){
        #print(cost(S,y,Z,dim,num))
        a <- 1/k
        c <- grad(S,y,Z,dim,num)

        #get new z
        Z <- Z - t(a*c)

        #add Z to Z_history
        Z_history <- cbind(Z_history,Z)

        k<-k+1
    }
    return(list(Z=Z, Z_history=Z_history))
}

cost <- function(x, y, z, dim, num){
    cc <- 0

    for(i in 1:num){
        #get ith row of x
        #x(,i)
        xx <- t(x[i,])

        #xx <- t(rbind(1,xx))
        #f(x) = sign((1,x), z)
        #fx <- sign(t(z) %*% xx)

        #print( (xx[,-3] %*% z[-3,]) - z[3,]) 


        fx <- sign( (xx[,-3] %*% z[-3,]) + z[3,] )

        dot <- (xx[,-3] %*% z[-3,]) + z[3,]

        id <- 1*(fx != y[i])
        cc <- cc + id*dot
    }
    return (cc=cc)
}

grad <- function(x, y, z, dim, num){
    g <- t(as.matrix(rep(0, dim)))
   #sum indic*(-y)(1,x) 
    for(i in 1:num){

        xx <- t(as.matrix(x[i,]))

        fx <- as.integer(sign( (xx[,-3] %*% z[-3,]) + z[3,] ))

        id <- as.integer(1*(fx != y[i]))

        negy <- -1*y[i]

        g <- g + id*negy*xx
    }

    return (g=g)
}
