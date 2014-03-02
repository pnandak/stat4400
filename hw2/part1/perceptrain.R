#inputs
#S: set of vectors to be trained around
#y: class labels

#Outputs
#z: hyperplane defining classifier
#Z_history: history of transformation of Z over iterations

perceptrain <- function(S,y){
 
    #S <- do.call(rbind, S[1])

    d <- nrow(S)
    #current hyperplane
    Z <- rep(0, d)
 
    #history list
    Z_history <- list()

    #iteration
    k <- 1 
 

    cost <- function(x,y,z, dim){
        cc <- 0
        for(i in 1:dim){
            #get ith col of x
            #x(,i)
            xx <- rbind(1,x(,i))
            #f(x) = sign((1,x), z)
            fx <- sign(xx %*% z)
            dot <- z%*%xx

            cc <- cc + 1*(fx == y[i])*dot
        }
        return (cc=cc)
    }

    grad <- function(x,y,z,dim){
        g <- rep(0, d)
       #sum indic*(-y)(1,x) 
        for(i in 1:dim){
            #get ith col of x
            #x(,i)
            xx <- rbind(1,x(,i))
            #f(x) = sign((1,x), z)
            fx <- sign(xx %*% z)


            g <- g + 1*(fx == y[i])*(-y[i])*(xx)
        }
        return (g=g)
    }



    while(!identical(cost(x,y,z,d),0)){
        a <- 1/k
        c <- grad(x,y,z,d)

        #get new z
        Z <- Z - a*c

        #add Z to Z_history
        Z_history <- cbind(Z_history,Z)

        k<-k+1
    }

    return(Z=Z, Z_history=Z_history)
}


