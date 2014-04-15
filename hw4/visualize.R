#inputs
#m: hard assignmnets for EM alg clusters

#Outputs
#none

visualize <- function(m, x, y){
	m_n<-matrix(m, nrow=x, ncol=y)
	m_n <- m_n[,ncol(m_n):1]
    image(m_n)

}
