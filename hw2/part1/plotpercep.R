#inputs
#S: set of vectors 
#Z: normal vector
#Z_history: history of normal vector
#y: classes

#Outputs

plotpercep <- function(S,Z){
    y <- do.call(rbind, S[2])
    S <- do.call(rbind, S[1])
    S <- S[,-3]
    S <- cbind(S,t(y))

    plot(S[,2], S[,1], col = as.integer(S[,3]+2))

    #normalize and plot Z
    Z_history <- do.call(rbind, Z[2])
    Z <- do.call(rbind, Z[1])
    Z_history <- matrix(unlist(Z_history), nr=nrow(Z) )

    Zx <- Z[1,]
    Zy <- Z[2,]
    Zc <- -1*Z[3,] *(sqrt(Zx^2 + Zy^2))

    Zr <- sqrt(Zx^2 + Zy^2)
    Zr_normalized <- Zr/Zc

    Zx_normalized <- (Zr_normalized)*(Zx/Zr)
    Zy_normalized <-  (Zr_normalized)*(Zy/Zr)

    Z_final <- c(Zx_normalized,Zy_normalized)


    normal_slope <- -Zx/Zy
    Z_other <- c(Zx_normalized + 5, Zy_normalized + 5*normal_slope )
    Z_other2 <- c(Zx_normalized - 5, Zy_normalized - 5*normal_slope )

    #plot(Zx_normalized, Zy_normalized)
    p <- cbind(Z_final, Z_other, Z_other2)
    lines(p[2,], p[1,], col = "red")


    #plot history of Z
    Z_history <-cbind(Z_history)

    for(i in 1:ncol(Z_history)){
        Zz <- t(t(Z_history[,i]))
        Zx <- Zz[1,]
        Zy <- Zz[2,]
        Zc <- Zz[3,]

        Zr <- sqrt(Zx^2 + Zy^2)
        Zr_normalized <- Zr/Zc 

        Zx_normalized <- (Zr_normalized)*(Zx/Zr)
        Zy_normalized <-  (Zr_normalized)*(Zy/Zr)

        Z_final <- c(Zx_normalized,Zy_normalized, -1)
        Z_history[,i] <- Z_final
    }
    plot(Z_history[1,], Z_history[2,])

}
