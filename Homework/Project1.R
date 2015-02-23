##When you first run these R codes, yous should install the packages if you did not install before.
##But when you installed packages, you need not to install again.


#Input support_data.csv
mydata<-read.csv("/Users/mlu/Courses/CPH576C/Data/support_data.csv",sep=",")
##install.packages("fields")
require(fields)


#Make a descripetive data analysis
summ1<-stats(mydata)
pro1.data<-mydata[,c("age","sex","dzgroup","num.co","scoma","race","meanbp","hrt","temp","pafi","alb","totcst")]
head(pro1.data)
summ2<-stats(pro1.data)
ana.data<-pro1.data[complete.cases(pro1.data),]#delete missing data
summ3<-stats(ana.data)
#pro1.data<-pro1.data[pro1.data$totcst==0]<-NA


#Delete zero value in totcst column
pro1.data<-subset(pro1.data,totcst!=0)
#pro1.data[pro1.data[,ncol(pro1.data)]!=0]


#Delete missing data
pro2.data<-subset(pro1.data,!is.na(totcst)) #select the subset of data frame


# Draw scatter plot matrix
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1ScatterplotMatrix.pdf")
pairs(pro2.data,main="Scatter Plot Matrix")
dev.off()


# Group mean
attach(pro2.data)
aggregate(totcst,by=list(sex),FUN=mean)
aggregate(totcst,by=list(dzgroup),FUN=mean)
aggregate(totcst,by=list(race),FUN=mean)


#Draw boxplot 
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1boxplot1.pdf")
layout(matrix(c(1,1,2,3),2,2,byrow=T))
boxplot(totcst~dzgroup,main="Boxplot of totcst and dzgroup",col="blue")
boxplot(totcst~sex,main="Boxplot of totcst and sex",col="lightgray")
boxplot(totcst~race,main="Boxplot of totcst and race",col="bisque")
dev.off()


# produce a graph of group mean and confidence interval
##install.packages("gplots")
require("gplots")
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1plotmean1.pdf")
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plotmeans(totcst~dzgroup,xlab="group", ylab="Total cost",main="Mean Plot\nwith 95% CI")
plotmeans(totcst~sex,xlab="Sex", ylab="Total cost",main="Mean Plot\nwith 95% CI")
plotmeans(totcst~race,xlab="Race", ylab="Total cost",main="Mean Plot\nwith 95% CI")
dev.off()


pdf("/Users/mlu/Courses/CPH576C/Homework/Project1boxplot2.pdf")
layout(matrix(c(1,1,2,3),2,2,byrow=T))
boxplot(log(totcst)~dzgroup,main="Boxplot of logtotcst and dzgroup",col="blue")
boxplot(log(totcst)~sex,main="Boxplot of logtotcst and sex",col="lightgray")
boxplot(log(totcst)~race,main="Boxplot of logtotcst and race",col="bisque")
dev.off()


# produce a graph of group mean and confidence interval
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1plotmean2.pdf")
layout(matrix(c(1,1,2,3),2,2,byrow=T))
plotmeans(log(totcst)~dzgroup,xlab="group", ylab="Total cost",main="Mean Plot\nwith 95% CI")
plotmeans(log(totcst)~sex,xlab="Sex", ylab="Total cost",main="Mean Plot\nwith 95% CI")
plotmeans(log(totcst)~race,xlab="Race", ylab="Total cost",main="Mean Plot\nwith 95% CI")
dev.off()
detach(pro2.data)


# Log transformation
log.totcst<-log(pro2.data$totcst)
pro3.data<-cbind(pro2.data,log.totcst)


# Draw histogram
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1histogram.pdf")
par(mfrow=c(1,2))
hist(pro2.data$totcst,xlab="Total Cost",col="red",main="Histogram of totcst");rug(jitter(pro2.data$totcst));lines(density(pro2.data$totcst), col="blue", lwd=2)
hist(pro3.data$log.totcst,xlab="Log Total Cost",col="red",main="Histogram of totcst");rug(jitter(pro3.data$log.totcst));lines(density(pro3.data$log.totcst), col="blue", lwd=2)
dev.off()


# Draw plot of the relationship between the dependent variable, the covariate and the factor
##install.packages("HH")
require("HH")
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1.pdf")
par(mfrow=c(6,4))
ancova(totcst~age+sex,data=pro2.data);ancova(totcst~age+dzgroup,data=pro2.data);ancova(totcst~age+race,data=pro2.data)
ancova(totcst~num.co+sex,data=pro2.data);ancova(totcst~num.co+dzgroup,data=pro2.data);ancova(totcst~num.co+race,data=pro2.data)
ancova(totcst~scoma+sex,data=pro2.data);ancova(totcst~scoma+dzgroup,data=pro2.data);ancova(totcst~scoma+race,data=pro2.data)
ancova(totcst~meanbp+sex,data=pro2.data);ancova(totcst~meanbp+dzgroup,data=pro2.data);ancova(totcst~meanbp+race,data=pro2.data)
ancova(totcst~hrt+sex,data=pro2.data);ancova(totcst~hrt+dzgroup,data=pro2.data);ancova(totcst~hrt+race,data=pro2.data)
ancova(totcst~temp+sex,data=pro2.data);ancova(totcst~temp+dzgroup,data=pro2.data);ancova(totcst~temp+race,data=pro2.data)
ancova(totcst~pafi+sex,data=pro2.data);ancova(totcst~pafi+dzgroup,data=pro2.data);ancova(totcst~pafi+race,data=pro2.data)
ancova(totcst~alb+sex,data=pro2.data);ancova(totcst~alb+dzgroup,data=pro2.data);ancova(totcst~alb+race,data=pro2.data)
dev.off()
#Draw scatter plot between predictors and response variable
attach(pro3.data)
pdf("/Users/mlu/Courses/CPH576C/Homework/scatterplot.pdf")
plot(num.co,log.totcst)
plot(scoma,log.totcst)
plot(meanbp,log.totcst)
plot(temp,log.totcst)
plot(pafi,log.totcst)
plot(alb,log.totcst)
plot(age,log.totcst)
plot(hrt,log.totcst)
dev.off()
detach(pro3.data)

#Dividing the predictor of scoma into three groups
pro3.data$Scoma[pro3.data$scoma>=0 & pro3.data$scoma <=20]<-"Scomalow"
pro3.data$Scoma[pro3.data$scoma>=21 & pro3.data$scoma <=70]<-"Scomamedian"
pro3.data$Scoma[pro3.data$scoma>=71 & pro3.data$scoma <=100]<-"Scomahigh"
require("Hmisc")

# Using Linear Regression Model to fit data
#rcs function in package Hmisc can do the cubic splines estimate
projectlm1 <- lm(log.totcst~age+factor(sex)+factor(dzgroup)+num.co+factor(Scoma)+factor(race)+rcs(meanbp,4)+rcs(hrt,4)+rcs(temp,4)+pafi+alb,data=pro3.data)
summary(projectlm1)
pdf("/Users/mlu/Courses/CPH576C/Homework/Projectlm1diagonose.pdf")
par(mfrow=c(2,2))
plot(projectlm1)
dev.off()



projectlm2 <- lm(log.totcst~age+factor(sex)*factor(dzgroup)*factor(race)+num.co+factor(Scoma)+rcs(meanbp,4)+rcs(hrt,4)+rcs(temp,4)+pafi+alb,data=pro3.data)
summary(projectlm2)
pdf("/Users/mlu/Courses/CPH576C/Homework/Projectlm2diagonose.pdf")
par(mfrow=c(2,2))
plot(projectlm2)
dev.off()


#There are some different between projectlm1 and projectlm3. In my opinion, sex, dzgroup, race should have interaction with age. For example, age:factor(dzgroup) means age and dzgroup interaction etc. With this model, The adjusted R squared equals 0.453, and the R square is equals to 0.515
projectlm3 <- lm(log.totcst~age+factor(sex)+age:factor(dzgroup)+factor(dzgroup)+num.co+factor(Scoma)+factor(race)+rcs(meanbp,4)+rcs(hrt,4)+rcs(temp,3)+pafi+alb,data=pro3.data)
summary(projectlm3)
pdf("/Users/mlu/Courses/CPH576C/Homework/Projectlm3diagonose.pdf")
par(mfrow=c(2,2))
plot(projectlm3)
dev.off()




# Some of the diagnoses
require("car")
confint(projectlm3)
summary(res<-residuals(projectlm3))
outlierTest(projectlm3)
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1influenceplot.pdf")
influencePlot(projectlm3)
dev.off()
durbinWatsonTest(projectlm3) #Test autocorrectlation between the ith sigma and jth sigma
vif(projectlm3)



# Draw a Hat Leverage Plot
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1hatplot.pdf")
hat.plot <- function(projectlm3) {
p <- length(coefficients(projectlm3))
n <- length(fitted(projectlm3))
plot(hatvalues(projectlm3), main="Index Plot of Hat Values")
abline(h=c(2,3)*p/n, col="red", lty=2)
identify(1:n, hatvalues(projectlm3), names(hatvalues(projectlm3)))
}
hat.plot(projectlm3)
dev.off()


# Draw a Cook's Distance Plot
pdf("/Users/mlu/Courses/CPH576C/Homework/Project1cook'sdplot.pdf")
cutoff <- 4/(nrow(pro3.data)-length(projectlm3$coefficients)-2)
plot(projectlm3, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")
dev.off()



# Delete outliers and influence and rebuild the model
pro4.data <-pro3.data[-c(253,697),]
projectlm4 <- lm(log.totcst~age+factor(sex)+age:factor(dzgroup)+factor(dzgroup)+num.co+factor(Scoma)+factor(race)+rcs(meanbp,4)+rcs(hrt,4)+rcs(temp,3)+pafi+alb,data=pro4.data)
summary(projectlm4)
pdf("/Users/mlu/Courses/CPH576C/Homework/Projectlm4diagonose.pdf")
par(mfrow=c(2,2))
plot(projectlm4)
dev.off()


# Using stepwise method to select significent predictors
##But, professor Bell said we should include all the predictors, so this step is not important
step.lm1 <- step(projectlm4)
summary(step.lm1)
pdf("/Users/mlu/Courses/CPH576C/Homework/Projectlm3stepwisediagonose.pdf")
par(mfrow=c(2,2))
plot(step.lm1)
dev.off()







##THIS PART IS NOT IMPORTANT. 


#require("Hmisc")
#projectlm1 <- lm(log.totcst~age+factor(sex)+factor(dzgroup)+num.co+scoma+factor(race)+meanbp+I(meanbp^2)+hrt+I(hrt^2)+temp+I(temp^2)+pafi+I(1/pafi)+alb,data=pro3.data)
#summary(projectlm1)
#pdf("/Users/mlu/Courses/CPH576C/Homework/Projectglm1diagonose.pdf")
#par(mfrow=c(2,2))
#plot(projectlm1)
#dev.off()





#lsmeans(projectglm2,trt.vs.ctrl1 ~ sex| dzgroup:race)
#summary(step(glm(log.totcst~age+factor(sex)*factor(dzgroup)*factor(race)+num.co+scoma+meanbp+hrt+temp+pafi+alb,family = gaussian,data=pro3.data)))
#boxcox(projectmodel1,lambda=seq(-2,2,1/10),plotit = TRUE)
#attach(pro3.data)
#cars.spl <- smooth.spline(meanbp,logtotcst)
#(cars.spl)
#detach(pro3.data)
#xx.meanbp <- rcspline.eval(pro3.data$meanbp, inclx=TRUE, nk=4)
#knots <- attr(xx.meanbp, "knots")
#coef <- lsfit(xx.meanbp, pro3.data$log.totcst)$coef
#options(digits=4)
# rcspline.restate must ignore intercept
#w.meanbp <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
# could also have used coef instead of coef[-1], to include intercept
#cat(attr(w,"latex"), sep="\n")
#xx.temp <- rcspline.eval(pro3.data$temp, inclx=TRUE, nk=4)
#knots <- attr(xx.temp, "knots")
#coef <- lsfit(xx.temp, pro3.data$log.totcst)$coef
#options(digits=4)
# rcspline.restate must ignore intercept
#w.temp <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
# could also have used coef instead of coef[-1], to include intercept
#cat(attr(w,"latex"), sep="\n")
#xx.hrt <- rcspline.eval(pro3.data$hrt, inclx=TRUE, nk=4)
#knots <- attr(xx.hrt, "knots")
#coef <- lsfit(xx.hrt, pro3.data$log.totcst)$coef
#options(digits=4)
# rcspline.restate must ignore intercept
#w.hrt <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
# could also have used coef instead of coef[-1], to include intercept
#cat(attr(w,"latex"), sep="\n")
#xx.pafi <- rcspline.eval(pro3.data$pafi, inclx=TRUE, nk=4)
#knots <- attr(xx.pafi, "knots")
#coef <- lsfit(xx.pafi, pro3.data$log.totcst)$coef
#options(digits=4)
# rcspline.restate must ignore intercept
#w.pafi <- rcspline.restate(knots, coef[-1], x="{\\rm BP}")
# could also have used coef instead of coef[-1], to include intercept
#cat(attr(w,"latex"), sep="\n")






