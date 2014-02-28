#inputs
#S: set of vectors to be classified
#z: z[1:d] is vector to defining hyperplane

#Outputs
#y: class label

classify <- function(S,z){

# obtain dimension
d <- length(z)-1
offset <- -z[length(z)] * z[1:d] / sum(z[1:d]^2)

# compute the class assignments
y <- as.vector(sign(S %*% w))


}
