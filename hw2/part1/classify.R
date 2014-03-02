#inputs
#S: set of vectors to be classified
#z: z[1:d] is vector to defining hyperplane

#Outputs
#y: class label

classify <- function(S,z){

SS <- do.call(rbind, S[1])

y = as.vector(sign((SS %*% z)))
return(y=y)
}
