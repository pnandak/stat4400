#Inputs

#Outputs

#runsvm("/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspsdata.txt","/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspscl.txt", "/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspsdata_test.txt", "/Volumes/Macintosh HD/Users/theocean154/Documents/School_files/College/Semester 8/STAT 4400/HW/stat4400/hw2/part2/uspscl_test.txt")


runsvm <- function(data,class){
    if(! require(e1071))
    {
        install.packages("e1071")
    }

    library(e1071)
    library(rpart)


    d <- read.table(data, header = FALSE, sep = "", skip = 0)
    c <- read.table(class, header = FALSE, skip = 0)

    d <- as.matrix(d)
    class <- as.matrix(c)
    colnames(class) <- "class"
    d <- data.frame(cbind(d,class))


    f <- as.formula(paste(tail(names(d), 1), "~ ."))

    index <- 1:nrow(d)
    testindex <- sample(index, trunc(length(index)/5))
    testset <- d[testindex,]
    trainset <- d[-testindex,]



    svm.model <- svm(f, data=trainset, cost=70, gamma = 1, cross =5)
    #model <- svm(data=x, x, y, cost=70, gamma = 0.003, cross=5)
    print(summary(svm.model))

    svm.pred <- predict(svm.model, testset[,-ncol(d)])

    #print(summary(pred))
    #print(attr(pred, "decision.values"))
    #plot(cmdscale(dist(testset)),
     # col = as.integer(pred<0)+1,
      #  pch = c("o","+")[1:150 %in% model$index + 1])

    tab <- table(pred = svm.pred, true = testset[,ncol(d)])

    print(tab)
    print(classAgreement(tab))
}