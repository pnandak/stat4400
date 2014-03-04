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
    d <- cbind(d,class)
    d <- data.frame(d)

    #f <- as.formula(paste(tail(names(d), 1), "~ ."))

    index <- 1:nrow(d)
    testindex <- sample(index, trunc(length(index)/5))
    testset <- d[testindex,]
    trainset <- d[-testindex,]

    x <- as.matrix(trainset[,-ncol(d)])
    y <- as.matrix(trainset[,ncol(d)])

    #gamma is margin parameter
    #cost is kernel bandwidth

    #linear
    #svm.model <- svm(class ~ ., data=trainset, cost=500, kernel = "linear", type="C-classification", cross=10)
    #rbf
    svm.model <- svm(class ~ ., data=trainset, cost=10000, gamma = .1, type="C-classification", cross=10)

    #model <- svm(data=x, x, y, cost=70, gamma = 0.003, cross=5)
    #print(summary(svm.model))

    svm.pred <- predict(svm.model, testset[,-ncol(d)], decision.values = TRUE)

    #print(summary(pred))

    tab <- table(pred = svm.pred, true = testset[,ncol(d)])

    #print(tab)
    print(classAgreement(tab))
}
