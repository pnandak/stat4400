#inputs
#H: matrix of input histograms
#K: number of clusters
#tau:threshold parameter

#Outputs
#m:hard assignment vector to be visualized

MultinomialEM <- function(H, K, tau){
    #H is ndata rows, nbins columns
    H_i<-H
    n <- nrow(H_i)
    dim <- ncol(H_i)
    H_i<-matrix(apply(H_i, 1:2, function(x){if(x<1){x<-x+0.01}else{x}}), ncol= ncol(H_i), nrow=nrow(H_i))

    print(H_i[1,])

    #centroids
    index <- 1:n
    centroids<- sample(index,K)
    t_k <- matrix(t(H_i[centroids,] ), ncol=K)

    #assignment probabilities
    a_n <- matrix(1.0/K, ncol=K, nrow=n)

    #mixture weights
    c_k <- matrix(1.0/K, ncol=1, nrow=K)

    #b
    #b_k <- matrix(0, ncol=K, nrow=1)

    #phi
    phi <- matrix(0.0, ncol=K, nrow=n)

    #delta
    delta <- tau + 100.0

    #hard assignments
    m_n <- matrix(1, ncol=1, nrow=n)



    while(delta >= tau) {
        #compute phi
        t_k <- matrix(apply(t_k, 1:2, function(x) log(x)), ncol=ncol(t_k), nrow=nrow(t_k))
        print("error before here")
        print(t_k)
        print(H_i[1,])

        #want n*k matrix
        phi <- H_i %*% t_k
        print("error before here")
        print(phi[1,])

        phi <- matrix(apply(phi, 1:2, function(x) exp(x)), nrow=nrow(phi), ncol=ncol(phi))
        print("error before here")
        print(phi[1,])

        #compute a
        a_n_old <- a_n
        #n*1 denominator 
        denom <- phi%*%c_k


        phi <- matrix(apply(phi, 1, function(x) x*c_k), nrow = nrow(phi), ncol=ncol(phi))


        #divide each element by the denominator
        for(i in 1:n){
            a_n[i,] <- phi[i,] * denom[i,]
        }



        #compute c
        tmp <- matrix(1, nrow=1, ncol=n)
        #sum over n
        numer <- tmp %*% a_n
        #scale by n
        c_k <- numer / n
        #numer is 1*k, we need k*1
        c_k <- t(c_k)




        #compute b
        #b is dim by K 
        b_k <- t(H_i) %*%a_n



        #compute t
        t_k <- matrix(apply(b_k, 1, function(x) x/sum(x, na.rm=FALSE)), nrow=nrow(t_k), ncol=ncol(t_k))




        #update delta
        nor <- a_n - a_n_old
        delta <- norm(nor, 'O')
        print("tau")
        print(tau)
        print("delta")
        print(delta)

    }

    #make hard assignments
    for( i in 1:n){
        m_n[i] <- which.max(a_n[i,])
    }

    return(m=m_n)
}

