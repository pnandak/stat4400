#Inputs

#Outputs

#runsvm("/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspsdata.txt","/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspscl.txt", "/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspsdata_test.txt", "/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspscl_test.txt")

runsvm <- function(data,class,data_test,class_test){

    d <- read.table(data, header = FALSE, sep = "", skip = 0)
    d_test <- read.table(data_test, header = FALSE, sep = "", skip = 0)
    c <- read.table(class, header = FALSE, skip = 0)
    c_test <- read.table(class_test, header = FALSE, skip = 0)

    d <- as.matrix(d)
    d_test  <- as.matrix(d_test)
    c <- as.matrix(c)
    c_test <- as.matrix(c_test)

    x <- d
    y <- c
    model <- svm(data=x,x,y, cost=70, gamma = .003)
    print(model)
    summary(model)

    pred <- predict(model, x)
    print(table(pred, y))
    plot(cmdscale(dist(d)),
      col = as.integer(pred<0)+1,
        pch = c("o","+")[1:150 %in% model$index + 1])


    pred <- predict(model, d_test, decision.values = TRUE)
    print(table(pred,c_test))
    attr(pred, "decision.values")
    plot(cmdscale(dist(d_test)),
      col = as.integer(pred<0)+1,
        pch = c("o","+")[1:150 %in% model$index + 1])

#    plot(cmdscale(dist(p[,-ncol(p)])),
 #       col = as.integer(p[,ncol(p)])+4,
  #      pch = c("o","+")[1:150 %in% model$index + 1])



}