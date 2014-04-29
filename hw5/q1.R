plotexp <- function(theta, x_max, n){

    expo <- function(x) {
        res<-theta*exp(-1*theta*x)
        return(res=res)
    }
    curve(expo, from = 0, to = x_max, n=n)


return()
}

