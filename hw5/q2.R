q2 <- function(){
    n<-256
    a<-2.0
    b<-0.2

    #generate 256 samples
    samples <- list(rep(0,n))
    for(i in 1:n){
        samples[i]<-rexp(1)
    }

    #use log gamma-->R function = lgamma(), take exp(lgamma)
    for(i in 1:n){
        a<-a+1
        b<-b+as.numeric(samples[i])
        if( (i==4) || (i==8) || (i==16) || (i==256)){
            posterior <- function(theta){
                res <- exp(  i*log(theta)  - theta*(b-.2) + (a-1)*theta +a*log(b) - b*theta - lgamma(a))
                return(res=res)
            }
            if(i==4){
                curve(posterior, from=0, to=4, n=500,col="red",xname="theta")
            }
            else if(i==8){
                curve(posterior, from=0, to=4, n=500,col="blue",add=TRUE,xname="theta")
            }
            else if(i==16){
                curve(posterior, from=0, to=4, n=500,col="green", add=TRUE,xname="theta")
            }
            else if(i==256){
                curve(posterior, from=0, to=4, n=500,col="black", add=TRUE,xname="theta")
            }
        }
    }


    #visualize posterior distr at n = {4,8,16,256}
    #comment on behavior as x increases



return()
}

