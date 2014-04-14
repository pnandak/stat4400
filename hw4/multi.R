#inputs
#H: matrix of input histograms
#K: number of clusters
#tau:threshold parameter

#Outputs
#m:hard assignment vector to be visualized

MultinomialEM <- function(H, K, tau){
    #H is ndata rows, nbins columns
    n <- nrow(H)
    dim <- ncol(H)

    #centroids
    index <- 1:n
    centroids<- sample(index,K)
    t_k <- matrix(t(H[centroids,] ), ncol=K)

    #assignment probabilities
    a_n <- matrix(0, ncol=1, nrow=n)

    #mixture weights
    c_k <- matrix(0, ncol=K, nrow=1)

    #b
    b <- matrix(0, ncol=K, nrow=1)

    #phi
    phi <- matrix(0, ncol=K, nrow=n)

    #delta
    delta <- 0

    #hard assignments
    m_n <- matrix(0, ncol=1, nrow=n)




    return(m=m_n)
}
