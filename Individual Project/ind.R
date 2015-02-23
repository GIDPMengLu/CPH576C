PCA.data1 <- PCA.data[,c("daysdrink","daysanysub","anysubstatus","drinkstatus")]
PCA1 <- prcomp(PCA.data1)
summary(PCA1)
require(caret)
trans1 <- preProcess(PCA.data1, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC1 <- predict(trans1, PCA.data1)
prediction1<- data.frame(PC1[,c(1:2)])
names(prediction1) <- c("PC1a","PC1b")



PCA.data2 <- PCA.data[,c("time","cesdtv","mcstv","g1btv","indtottv","drugrisktv","sexrisktv")]
PCA2 <- prcomp(PCA.data2)
summary(PCA2)
require(caret)
trans2 <- preProcess(PCA.data2, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC2 <- predict(trans2, PCA.data2)
prediction2<- data.frame(PC2[,c(1:2)])
names(prediction2) <- c("PC2a","PC2b")


PCA.data3 <-PCA.data[,c("a15b","d1","f1a","f1b","f1c","f1d","f1e","f1f","f1g","f1h","f1i","f1j","f1k","f1l","f1m","f1n","f1o","f1p","f1q","f1r","f1s","f1t","i2","pss_fr","i1tv","pcstv")]
PCA3 <- prcomp(PCA.data3)
summary(PCA3)
trans3 <- preProcess(PCA.data3, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC3 <- predict(trans3, PCA.data3)
prediction3<- data.frame(PC3[,c(1:3)])
names(prediction3) <- c("PC3a","PC3b","PC3c")




PCA.data4 <-PCA.data[,c("a15a","dayslink","linkstatus","satreat")]
PCA4 <- prcomp(PCA.data4)
summary(PCA4)
trans4 <- preProcess(PCA.data4, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC4 <- predict(trans4, PCA.data4)
prediction4<- data.frame(PC4[,1])
names(prediction4) <- c("PC4a")

data1 <- cbind(prediction1,prediction2,prediction3,prediction4)
data2 <- subset(long.data1,select=c(age,e2btv,id,homeless,substance,racegrp,female,treat))
data3 <- cbind(data1,data2)
