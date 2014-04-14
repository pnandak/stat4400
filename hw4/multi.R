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
    t_k <- matrix(, ncol=K, nrow=dim)

    #assignment probabilities
    a_n <- matrix(, ncol=1, nrow=n)

    #mixture weights
    c_k <- matrix(, ncol=K, nrow=1)

    #b
    b <- matrix(, ncol=K, nrow=1)

    #phi
    phi <- matrix(, ncol=K, nrow=n)

    #delta
    delta <- 0

    #hard assignments
    m_n <- matrix(, ncol=1, nrow=n)


    return(m=m_n)
}
