#Input help data frame
mydata <- read.table("/Users/mlu/Courses/CPH576C/Data/help_data.csv",header=T,sep=",")
#head(mydata)


#############################################################
##########################Summary statistics####################
#############################################################
#Five number summary and the missing values
require(fields)
stats(mydata)
require(pastecs)
stat.desc(mydata$e2b) #take a look if the mean and the variance are equal


#################################################################
###########################Primary analysis#########################
#################################################################
data1 <- mydata[1:386,]
data2 <- data1[,c("age","anysubstatus","cesd","cesd1","cesd2","cesd3","cesd4","d1","e2b","e2b1","e2b2","e2b3","e2b4","female","g1b","g1b1","g1b2","g1b3","g1b4","homeless","i1","i11","i12","i13","i14","id","indtot","indtot1","indtot2","indtot3","indtot4","linkstatus","satreat","sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4","substance","treat")]

long.data1 <- reshape(data2,idvar=c("id"),varying=list(c("cesd","cesd1","cesd2","cesd3","cesd4"),c("i1","i11","i12","i13","i14"),c("g1b","g1b1","g1b2","g1b3","g1b4"),c("e2b","e2b1","e2b2","e2b3","e2b4"),c("indtot","indtot1","indtot2","indtot3","indtot4"),c("sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4")),v.names=c("cesdtv","i1tv","g1btv","e2btv","indtottv","sexrisktv"),timevar="time",times=0:4,direction="long")


long.data2 <- na.omit(long.data1)
require(grDevices)
tN<- table(long.data2$e2btv)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/barplot.pdf")
r <- barplot(tN, col = rainbow(20))
dev.off()


long.data2 <- within(long.data2, {
      anysubstatus <- as.factor(anysubstatus)
      female <- as.factor(female)
      homeless <- as.factor(homeless)
      linkstatus <- as.factor(linkstatus)
      satreat <- as.factor(satreat)
      group <- as.factor(treat)
      time <- as.factor(time)
      glbtv <- as.factor(g1btv)
      id <- as.factor(id)
})

pdf("/Users/mlu/Courses/CPH576C/IndividualProject/zeroinflation.pdf")
ggplot(long.data2, aes(e2btv)) + geom_histogram() + scale_x_log10()
dev.off()
#########################
#repeated measure analysis plot
#########################
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/interactionplot.pdf")
par(cex=0.6)
with(long.data2,interaction.plot(time,group,e2btv,lty=c(1,2,3,4),lwd=3,col=c("chartreuse","blue"),trace.label="group"))
dev.off()

library(nlme)
longg <- groupedData( e2btv~ as.numeric(group) * as.numeric(time) | id, data = data2)
fit.cs <- gls( fact_t2~ group * time, data = longg,
  corr = corCompSymm(, form= ~ 1 | id) )
summary(fit.cs)
intervals(fit.cs)
confint(fit.cs)

fit.un <- gls(e2btv~ group * time, data = longg,
  corr=corSymm(form = ~ 1 | id),
  weights = varIdent(form = ~ 1 | time))
summary(fit.un)
intervals(fit.un)
anova(fit.un)
confint(fit.un)


fit.ar1 <- gls(e2btv~ group * time, data = longg,
  corr = corAR1(, form= ~ 1 | id))
summary(fit.ar1)
intervals(fit.ar1)
anova(fit.ar1)
confint(fit.ar1)



fit.arh1 <- gls(e2btv~ group * time, data = longg,
  corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
intervals(fit.arh1)
anova(fit.arh1)
confint(fit.arh1)



anova(fit.cs, fit.un)
anova(fit.cs, fit.ar1)
anova(fit.cs, fit.arh1)
anova(fit.un, fit.arh1)


time.linear <- lme(e2btv~ group * time,random = list(id = pdDiag(~ time)), data = data2)
summary(time.linear)
anova(time.linear)

fitted <- fitted(time.linear, level=0)
require(lattice)
par(cex=0.6)
xyplot(e2btv~time,data=long.data2,groups=group,type="o",panel=panel.superpose)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/meanplotbygroup.pdf")
xyplot( e2btv~ time |group, data = long.data2, groups = id,type = "o", panel = panel.superpose)
dev.off()

xyplot(e2btv[group==1] ~ time[group==1], data = long.data2, groups = id,
  type = "o",panel = panel.superpose, col = "blue")
with(long.data2, lines(time[group==1], fitted[group==1],type = "b", col = "dark blue", lwd = 4))


xyplot(e2btv[group==2] ~ time[group==2], data = long.data2, groups = id,
  type = "o", panel = panel.superpose, col = "blue")
with(long.data2, lines(time[group==1], fitted[group==1],
  ylim = c(50, 150),  xlim = c(0, 800),
  type = "b", col = "dark red", lwd = 4))



#Generalized linear mixed model
#Poisson Regression
require(glmmADMB)
glmres1 <- glmer(e2btv~age+anysubstatus+d1+female+homeless*group+time+time:group+linkstatus+satreat+substance+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data2,family="poisson")
summary(glmres1)

glmres2 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless*group+time+time:group+linkstatus+satreat+substance+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data2,zeroInflation=T,family="poisson")
summary(glmres2)

glmres3 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless+time+time:group+linkstatus+satreat+substance*group+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data2,family="poisson")
summary(glmres3)

glmres4 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless+time+time:group+linkstatus+satreat+substance*group+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data2,zeroInflation=T,family="poisson")
summary(glmres4)


#Contingencey Table
#Case-control study
#Sensitivity and specificity
#xtres1 <- xtabs(~homeless+treat,data=mydata)
#xtres2 <- xtabs(~substance+treat,data=mydata)
#xtres3 <- xtabs(~homeless+substance+treat,data=mydata)
#ftable.xtres3 <- ftable(xtres3)


#Cochran–Mantel–Haenszel chi-square test
#mantelhaen1 <- mantelhaen.test(xtres1)
#mantelhaen2 <- mantelhaen.test(xtres2)
#mantelhaen3 <- mantelhaen.test(xtres3)


#Measures of association
#require(vcd)
#asso1 <- assocstats(xtres1)
#asso2 <- assocstats(xtres1[,,2])
#asso3 <- assocstats(xtres1[,,3])
#asso4 <- assocstats(xtres2)
#asso5 <- assocstats(xtres2[,,2])
#asso6 <- assocstats(xtres2[,,3])
#asso7 <- assocstats(xtres3[,,1])
#asso8 <- assocstats(xtres3[,,2])


#Converting the numeric variables to factor variables
data1 <- mydata[1:386,]
data2 <- data1[,c("age","anysubstatus","cesd","cesd1","cesd2","cesd3","cesd4","d1","e2b","e2b1","e2b2","e2b3","e2b4","female","g1b","g1b1","g1b2","g1b3","g1b4","homeless","i1","i11","i12","i13","i14","id","indtot","indtot1","indtot2","indtot3","indtot4","linkstatus","satreat","sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4","substance","treat")]


#######Missing values Imputation##########
help.data <- mydata[,c("id","e2b","e2b1","e2b2","e2b3","e2b4","i1","i11","i12","i13","i14","pcs","pcs1","pcs2","pcs3","pcs4","mcs","mcs1","mcs2","mcs3","mcs4","sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4","d1","age","substance","racegrp","cesd","cesd1","cesd2","cesd3","cesd4")]
help.data <- subset(mydata,select=-c(anysubstatus,female,g1b,g1b1,g1b2,g1b3,g1b4,homeless,linkstatus,satreat,treat))
attach(mydata)
anysubstatus<- as.factor(anysubstatus)
female <- as.factor(female);g1b <- as.factor(g1b)
g1b1 <- as.factor(g1b1);g1b2 <- as.factor(g1b2)
g1b3 <- as.factor(g1b3);g1b4 <- as.factor(g1b4)
homeless <- as.factor(homeless);linkstatus <- as.factor(linkstatus)
satreat <- as.factor(satreat);treat <- as.factor(treat)
detach(mydata)
help.data <- cbind(help.data,anysubstatus,female,g1b,g1b1,g1b2,g1b3,g1b4,homeless,linkstatus,satreat,treat)





#Display patterns of missing variables in a given data frame
require(mice);newdata <- help.data[1:386,]
missing.pattern <- md.pattern(help.data)



#Dealing with missing data by using multiple imputation method
#Because the multiple imputation method is randomized, I set the seed to conform the same results
imp1 <-mice(newdata,m=5,maxit=2,seed=23109,
           method=c(rep("pmm",74),rep("polyreg",2),rep("logreg",12)))
summary(imp1)
#Geting the complete data frame, there is no missing data any more.
complete.data1 <- complete(imp1)


selected.data <- complete.data1[,c("age","anysubstatus","cesd","cesd1","cesd2","cesd3","cesd4","d1","e2b","e2b1","e2b2","e2b3","e2b4","female","g1b","g1b1","g1b2","g1b3","g1b4","homeless","i1","i11","i12","i13","i14","id","indtot","indtot1","indtot2","indtot3","indtot4","linkstatus","satreat","sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4","substance","treat")]






#Inspecting the distributions of original and the imputed data
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/stripplot")
stripplot(imp1,pch=20,cex=1.2)
dev.off()

#Converting from wide form to long form
long.data3 <- reshape(selected.data ,idvar=c("id"),varying=list(c("cesd","cesd1","cesd2","cesd3","cesd4"),c("i1","i11","i12","i13","i14"),c("g1b","g1b1","g1b2","g1b3","g1b4"),c("e2b","e2b1","e2b2","e2b3","e2b4"),c("indtot","indtot1","indtot2","indtot3","indtot4"),c("sexrisk","sexrisk1","sexrisk2","sexrisk3","sexrisk4")),v.names=c("cesdtv","i1tv","g1btv","e2btv","indtottv","sexrisktv"),timevar="time",times=0:4,direction="long")

long.data3 <- within(long.data3, {
      anysubstatus <- as.factor(anysubstatus)
      female <- as.factor(female)
      homeless <- as.factor(homeless)
      linkstatus <- as.factor(linkstatus)
      satreat <- as.factor(satreat)
      group <- as.factor(treat)
      time <- as.factor(time)
      glbtv <- as.factor(g1btv)
      id <- as.factor(id)
})


#Generalized mixed effect model
#Poisson Regression
require(glmmADMB)
glmres5 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless*group+time+time:group+linkstatus+satreat+substance+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data3,family="poisson")
summary(glmres5)

glmres6 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless*group+time+time:group+linkstatus+satreat+substance+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data3,zeroInflation=T,family="poisson")
summary(glmres6)

glmres7 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless+time+time:group+linkstatus+satreat+substance*group+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data3,family="poisson")
summary(glmres7)

glmres8 <- glmmadmb(e2btv~age+anysubstatus+d1+female+homeless+time+time:group+linkstatus+satreat+substance*group+cesdtv+i1tv+g1btv+indtottv+sexrisktv+(1|id),data=long.data3,zeroInflation=T,family="poisson")
summary(glmres8)








#########################################################
NN <- 200
sss1 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres1))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1930,replace=T),]
       glmres1 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1),data=sample.data,family="poisson")
        
        coef.beta <- coef(glmres1)
        sss1[ii,] <- coef.beta
    }
mean.beta1 <- apply(sss1,2,mean)  
sd.beta1 <- apply(sss1,2,sd)




data.beta1 <- rbind(coef.beta.1,mean.beta1,sd.beta1)


sss2 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres2))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
        glmres2 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1),zeroInflation=T,data=sample.data,family="nbinom")
        
        coef.beta <- coef(glmres2)
        sss2[ii,] <- coef.beta
    }
mean.beta2 <- apply(sss2,2,mean)  
sd.beta2 <- apply(sss2,2,sd)
data.beta2 <- rbind(coef.beta.2,mean.beta2,sd.beta2)

sss3 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres3))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
        glmres3 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+female+substance*treat+racegrp+(1|id1),data=sample.data,family="poisson")
        
        coef.beta <- coef(glmres3)
        sss3[ii,] <- coef.beta
    }
mean.beta3 <- apply(sss3,2,mean)  
sd.beta3 <- apply(sss3,2,sd)
data.beta3 <- rbind(coef.beta.3,mean.beta3,sd.beta3)



sss4 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres4))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
        glmres4 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+substance*treat+female+racegrp+(1|id1),zeroInflation=T,data=sample.data,family="nbinom")
        

        coef.beta <- coef(glmres4)
        sss4[ii,] <- coef.beta
    }
mean.beta4 <- apply(sss4,2,mean)  
sd.beta4 <- apply(sss4,2,sd)
data.beta4 <- rbind(coef.beta.4,mean.beta4,sd.beta4)

sss5 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres5))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
         glmres5 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1),data=data3,family="poisson")
        
        coef.beta <- coef(glmres5)
        sss5[ii,] <- coef.beta
    }
mean.beta5 <- apply(sss5,2,mean)
sd.beta5 <- apply(sss5,2,sd)
data.beta5 <- rbind(coef.beta.5,mean.beta5,sd.beta5)


sss6 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres6))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
         glmres6 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1),data=data3,family="nbinom")
        
        coef.beta <- coef(glmres6)
        sss6[ii,] <- coef.beta
    }
mean.beta6 <- apply(sss6,2,mean)  
sd.beta6 <- apply(sss6,2,sd)
data.beta6 <- rbind(coef.beta.6,mean.beta6,sd.beta6)


sss7 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres7)))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
        glmres7 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+female+substance*homeless*treat+racegrp+(1|id1),data=data3,family="poisson")
summary(glmres7)
       
        coef.beta <- coef(glmres7)
        sss7[ii,] <- coef.beta
    }
mean.beta7 <- apply(sss7,2,mean)  
sd.beta7 <- apply(sss7,2,sd)
data.beta7 <- rbind(coef.beta.7,mean.beta7,sd.beta7)
                   

sss8 <- data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres8))))
for(ii in 1:NN)
    {
        sample.data <- sssss[sample(nrow(sssss),1000),]
        glmres8 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+female+substance*homeless*treat+racegrp+(1|id1),data=data3,family="nbinom")
summary(glmres8)
        
        coef.beta <- coef(glmres8)
        sss8[ii,] <- coef.beta
    }
mean.beta8 <- apply(sss8,2,mean)  
sd.beta8 <- apply(sss8,2,sd)
data.beta8 <- rbind(coef.beta.8,mean.beta8,sd.beta8)






stderrs <- function(model)
    {
        ifelse(min(diag(vcov(model)))>0,
               sqrt(diag(vcov(model))),NA)
    }
ciout <- function(est,se,target)
    {
        ifelse((est-1.96*se>target) | (est+1.96*se<target),1,0)
    }
testpnb2 <- function(beta,data)
    {
        data <- sssss
        #sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
        #Creating some data
        sample.data <- sssss[sample(nrow(data),1930,replace=T),]

        #require(kimisc)
        #sample.rows(sssss,1930)
        
        # fit models to different family
       glmres3 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+female+substance*treat+racegrp+(1|id1),data=sample.data,family="poisson")
        glmres4 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+substance*treat+female+racegrp+(1|id1),zeroInflation=T,data=sample.data,family="nbinom")
        
         # extract parameter estimates using the coef() function
         est = as.numeric(c(coef(glmres3)[2], coef(glmres4)[2]))
         # use our two new functions to get the SE and the CI indicator
         se = c(stderrs(glmres3), stderrs(glmres4))
         ci = ciout(est, se, rep(beta,2))
         return(matrix(c(est,se,ci),ncol=3))
    }

testpnb1 <- function(n,beta,data)
    {
        sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
        #Creating some data
        sample.data <- sample.df(sssss, 1930)

        #require(kimisc)
        #sample.rows(sssss,1930)
        
        # fit models to different family
       glmres1 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1),data=sample.data,family="poisson")
        #glmres2 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1),zeroInflation=T,data=sample.data,family="nbinom")
        
         # extract parameter estimates using the coef() function
         est = as.numeric(c(coef(glmres1)[2], coef(glmres2)[2]))
         # use our two new functions to get the SE and the CI indicator
         se = c(stderrs(glmres1), stderrs(glmres2))
         ci = ciout(est, se, rep(beta,2))
         return(matrix(c(est,se,ci),ncol=3))
    }


testpnb3 <- function(n,beta,data)
    {
        sample.df <- function(df, n) df[sample(nrow(df), n), , drop = FALSE]
        #Creating some data
        sample.data <- sample.df(sssss, 1930)

        #require(kimisc)
        #sample.rows(sssss,1930)
        
        # fit models to different family
       glmres5 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1),data=data3,family="poisson")
        glmres6 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1),zeroInflation=T,data=sssss,family="nbinom")
        
         # extract parameter estimates using the coef() function
         est = as.numeric(c(coef(glmres1)[2], coef(glmres2)[2]))
         # use our two new functions to get the SE and the CI indicator
         se = c(stderrs(glmres1), stderrs(glmres2))
         ci = ciout(est, se, rep(beta,2))
         return(matrix(c(est,se,ci),ncol=3))
    }


NN<-10
sss1<-data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres1))))
rsq1 <- function(data)
    {  
        sample.data <- data[sample(nrow(data),1930,replace=T),]
        glmres1 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1),data=sample.data,family="poisson")
        coef.beta <- coef(glmres1)
        sss1[ii,] <- coef.beta  
        return(sss1)
    }
NN<-300
sss3<-data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres3))))
foreach(ii=1:NN,.combine=rbind)%dopar%
               rsq1(sssss)

sss3<-data.frame(matrix(0,nrow=NN,ncol=length(coef(glmres3))))
foreach(ii=1:NN,.combine=rbind)%dopar%
     {  
        sample.data <- sssss[sample(nrow(sssss),1930,replace=T),]
        glmres3 <- glmmadmb(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+treat*substance+female+racegrp+(1|id1),data=sample.data,family="poisson")
        coef.beta <- coef(glmres3)
        sss3[ii,] <- coef.beta  
     }
                   


                   

                   
rsq2 <- function(formula,data,indices)
    {
        d <- data[indices,]
        glmres <- glmmadmb(formula,data=d,family="nbinom")
        return(summary(glmres))
    }

require(boot)
results1.1 <- boot(data=sssss,,statistic=rsq1,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1))
results1.2 <- boot(data=sssss,,statistic=rsq2,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id1))


results2.1 <- boot(data=sssss,,statistic=rsq1,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+female+substance*treat+racegrp+(1|id1))
results2.2 <- boot(data=sssss,,statistic=rsq2,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless+female+substance*treat+racegrp+(1|id1))


results3.1 <- boot(data=sssss,,statistic=rsq1,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1))
results3.2 <- boot(data=sssss,,statistic=rsq2,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless+homeless:treat+substance*treat+racegrp+(1|id1))


results4.1 <- boot(data=sssss,,statistic=rsq1,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless*substance*treat+racegrp+(1|id1))
results4.2 <- boot(data=sssss,,statistic=rsq2,R=300,formula=e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age*female+homeless*substance*treat+racegrp+(1|id1))
#require(lme4)
#glmres1 <- glmer(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+age+homeless*treat+female+substance+racegrp+(1|id),data=data3,family="poisson")
#summary(glmres1)

#glmres2 <- glmer(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+PC3d+age+homeless+female+substance*treat+racegrp+(1|id),data=data3,family="poisson")
#summary(glmres2)


#glmres3 <- glmer(e2btv~PC1a+PC1b+PC2a+PC2b+PC3a+PC3b+PC3c+PC3d+age+homeless+homeless:treat+female+substance*treat+racegrp+(1|id),data=data3,family="poisson")
#summary(glmres3)
       

#Bayesian Poisson Regression
#require(MCMCpack)
#posterior1 <- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+age+treat+homeless+satreat+drinkstatus+linkstatus+female+substance+racegrp+anysubstatus+cesdtv,data=newcomplete.data1,family="poisson")
#summary(posterior1)
#posterior3 <- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+treat+homeless:substance+satreat+drinkstatus+linkstatus+female+age+racegrp+anysubstatus+cesdtv,data=newcomplete.data1,family="poisson")
#summary(posterior3)
#posterior3 <- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+treat+homeless:substance+satreat+drinkstatus+linkstatus+female:age+racegrp+substance*age+anysubstatus+cesdtv,data=newcomplete.data1,family="poisson")
#summary(posterior5)




#posterior2 <- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+age+treat+homeless+satreat+drinkstatus+linkstatus+female+substance+racegrp+anysubstatus+cesdtv,data=newcomplete.data2,family="poisson")
#summary(posterior2)
#posterior4 <- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+treat+homeless:substance+satreat+drinkstatus+linkstatus+age+female+racegrp+anysubstatus+cesdtv,data=newcomplete.data2,family="poisson")
#summary(posterior4)
#posterior6<- MCMCpoisson(e2btv~id+glbtv+iltv+mcstv+indtottv+sexrisktv+pcrectv+dl+treat+homeless:substance+satreat+drinkstatus+linkstatus:age+female+racegrp+substance*age+anysubstatus+cesdtv,data=newcomplete.data2,family="poisson")
#summary(posterior6)







#Generalized linear mixed model
#Poisson Regression
#require(lme4)
#glmres1 <- glmer(e2btv~g1btv+i1tv+indtottv+sexrisktv+d1+age+homeless+satreat+linkstatus+female+substance+racegrp+anysubstatus+cesdtv+(1|id)+(1|treat),data=newcomplete.data1,family="poisson")
#summary(glmres1)
#glmsres3 <- glmer(e2btv~g1btv+i1tv+indtottv+sexrisktv+d1+homeless*substance+satreat+linkstatus+female+age+racegrp+anysubstatus+cesdtv+(1|id),data=newcomplete.data1,family="poisson")
#summary(glmres3)
#glmres5 <- glmer(e2btv~g1btv+i1tv+indtottv+sexrisktv+d1+homeless*substance+satreat+linkstatus+female:age+racegrp+anysubstatus+cesdtv+(1|id)+(1|treat),data=newcomplete.data1,family="poisson")
#summary(glmres5)



#glmsres2 <- glmmadmb(e2btv~g1btv+i1tv+indtottv+sexrisktv+pcrectv+d1+age+homeless+satreat+linkstatus+female+substance+racegrp+anysubstatus+cesdtv+(1|id)+(1|treat),data=newcomplete.data2,family="poisson")
#summary(glmres2)
#glmres4 <- glmer(e2btv~g1btv+i1tv+indtottv+sexrisktv+pcrectv+d1+homeless*substance+satreat+linkstatus+age+female+racegrp+anysubstatus+cesdtv+(1|id)+(1|treat),data=newcomplete.data2,family="poisson")
#summary(glmres4)
#glmres6 <- glmer(e2btv~g1btv+i1tv+indtottv+sexrisktv+pcrectv+d1+homeless*substance+satreat+linkstatus*age+female+racegrp+anysubstatus+cesdtv+(1|id)+(1|treat),data=newcomplete.data2,family="poisson")
#summary(glmres6)





#imp2 <- mice(newdata,m=50,maxit=25,seed=23109)
#summary(imp2)
#print(imp2)
#complete.data2 <- complete(imp2)


                   #################################################################
###########################Cluster analysis#########################
#################################################################




#I need to use Principle component analysis, so I convert factor variables to numeric variables
#help.data <- subset(selected.data,select=-c(anysubstatus,g1b,g1b1,g1b2,g1b3,g1b4,linkstatus,satreat,))
#anysubstatus<- as.numeric(complete.data1$anysubstatus)
#drinkstatus <- as.numeric(complete.data1$drinkstatus)
#g1b <- as.numeric(complete.data1$g1b)
#g1b1 <- as.numeric(complete.data1$g1b1)
#g1b2 <- as.numeric(complete.data1$g1b2)
#g1b3 <- as.numeric(complete.data1$g1b3)
#g1b4 <- as.numeric(complete.data1$g1b4)
#linkstatus <- as.numeric(complete.data1$linkstatus)
#satreat <- as.numeric(complete.data1$satreat)
#subset.data1 <- data.frame(anysubstatus,drinkstatus,g1b,g1b1,g1b2,g1b3,g1b4,linkstatus,satreat)
#subset.data2 <- cbind(help.data,subset.data1)
PCA.data <- subset(long.data1,select=-c(age,pcrec1,pcrec2,pcrec3,pcrec4,e2btv,id,homeless,female,substance,racegrp,treat))
#Cluster analysis
require(ClustOfVar)
tree <- hclustvar(PCA.data)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/Clustertree.pdf")
plot(tree)
dev.off()
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/StabilityCluster.pdf")
stability(tree,B=40)
part1 <- cutreevar(tree,4)
print(part1)
dev.off()
summary(part1)
stability(tree,B=40)
part2 <- cutreevar(tree,3)
print(part2)
summary(part2)
#######################################################################


#######################################################################
#############################PCA and PCA regression#####################
######################################################################
PCA.data1 <- PCA.data[,c("daysdrink","daysanysub","anysubstatus","drinkstatus")]
Group <- long.data1[,c("treat")]
require(graphics)
PCA1 <- prcomp(PCA.data1);summary(PCA1);print(PCA1)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCAtree1.pdf")
plot(PCA1,type="l")
dev.off()
require(devtools)
#install_github("ggbiplot", "vqv")
require(ggbiplot)
attach(PCA.data1)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCA1.pdf")
g1 <- ggbiplot(PCA1,obs.scale=1,var.scale=1,groups=Group,ellipse=T,circle=T)
g1 <- g1 + scale_color_discrete(name = '')
g1 <- g1 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g1)
dev.off()
require(caret)
trans1 <- preProcess(PCA.data1, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC1 <- predict(trans1, PCA.data1);prediction1<- data.frame(PC1[,1:2]);names(prediction1) <- c("PC1a","PC1b")
#########################################
#########################################
PCA.data2 <- PCA.data[,c("a15a","dayslink","linkstatus","satreat","time","cesdtv","mcstv","g1btv","indtottv","drugrisktv","sexrisktv")]
PCA2 <- prcomp(PCA.data2);summary(PCA2);print(PCA2)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCAtree2.pdf")
plot(PCA2,type="l")
dev.off()
attach(PCA.data2)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCA2.pdf")
g2 <- ggbiplot(PCA2,obs.scale=1,var.scale=1,groups=Group,ellipse=T,circle=T)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g2)
dev.off()
trans2 <- preProcess(PCA.data2, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC2 <- predict(trans2, PCA.data2);prediction2<- data.frame(PC2[,1:2]);names(prediction2) <- c("PC2a","PC2b")
#######################################
#########################################
PCA.data3 <- PCA.data[,c("a15b","d1","f1a","f1b","f1c","f1d","f1e","f1f","f1g","f1h","f1i","f1j","f1k","f1l","f1m","f1n","f1o","f1p","f1q","f1r","f1s","f1t","i2","pss_fr","i1tv","pcstv")]
PCA3 <- prcomp(PCA.data3);summary(PCA3);print(PCA3)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCAtree3.pdf")
plot(PCA3,type="l")
dev.off()
#install_github("ggbiplot", "vqv")
attach(PCA.data3)
pdf("/Users/mlu/Courses/CPH576C/IndividualProject/PCA3.pdf")
g3 <- ggbiplot(PCA3,obs.scale=1,var.scale=1,groups=Group,ellipse=T,circle=T)
g3 <- g3 + scale_color_discrete(name = '')
g3 <- g3 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g3)
dev.off()
trans3 <- preProcess(PCA.data3, 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC3 <- predict(trans3, PCA.data3);prediction3<- data.frame(PC3[,c(1:3)]);names(prediction3) <- c("PC3a","PC3b","PC3c")
data1 <- cbind(prediction1,prediction2,prediction3);data2 <- long.data1[,c("age","e2btv","id","homeless","substance","racegrp","female","treat")]
data3 <- cbind(data1,data2)




#######################################################
#compute a coefficient of determination (R2), or an analogue, for (G)LMMs
r2.corr.mer <- function(m) {
   lmfit <-  lm(model.response(model.frame(m)) ~ fitted(m))
   summary(lmfit)$r.squared
}
1-var(residuals(m))/(var(model.response(model.frame(m)))
r2.corr.mer(glmres1)

#test for overdispersion/compute an overdispersion factor
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

library(lme4)  ## 1.0-4
set.seed(101)  
d <- data.frame(y=rpois(1000,lambda=3),x=runif(1000),
                f=factor(sample(1:10,size=1000,replace=TRUE)))
m1 <- glmer(y~x+(1|f),data=d,family=poisson)
overdisp_fun(m1)


library(glmmADMB)  ## 0.7.7
m2 <- glmmadmb(y~x+(1|f),data=d,family="poisson")
overdisp_fun(m2)
                   


