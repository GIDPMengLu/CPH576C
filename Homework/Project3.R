#require("sas7bdat")
#mydata <- read.sas7bdat("/Users/mlu/Courses/CPH576C/Data/Lung_sas.sas7bdat",debug=F)

######Pulse = γ00 + γ01(Exertype) + γ10(Time) + γ11(Exertype*time) + [ u0j + u1j(Time) + rij ]
require("aod")
mydata <- read.dta("/Users/mlu/Courses/CPH576C/Data/Lung_stata.dta")
require(fields)
stats(mydata)
data3 <- mydata[,c("funo","trtment","fact_t2","patid","exptx")]
data1 <- na.omit(data3)
data2 <- within(data1,{
     group <- as.factor(exptx)
     time <- as.factor(funo)
     id <- as.factor(patid)
     })


######################################
data3 <- subset(data3,patid<=10)
data2 <- within(data3,{
     group <- as.factor(trtment)
     time <- as.factor(funo)
     id <- as.factor(patid)
     })




data4 <- data3[is.na(data3$fact_t2)==T,]
data4$fact_t2<-0
data5<-na.omit(data3)
data6<-rbind(data4,data5)
data6<-data6[order(data6$patid),]

data7 <- within(data6,{
     group <- as.factor(exptx)
     time <- as.factor(funo)
     id <- as.factor(patid)
     })






par(cex=0.6)
with(data2,interaction.plot(time,group,fact_t2,lty=c(1,2,3,4),lwd=3,col=c("chartreuse","blue","brown"),trace.label="group"))
par(new=T)
with(data7,interaction.plot(time,group,fact_t2,lty=c(1,2,3,4),lwd=3,col=c("chartreuse","blue","brown"),trace.label="group"))
###Q1
tab1 <- table(data2$group,data2$time)
tab2 <- table(data7$group,data7$time)
difference <- tab2-tab1
library(gmodels)
CrossTable(difference)
require(doBy)
summary(fact_t2~time+group,data=data2,FUN=list(mean,sd))


###Q2
require(car)
data1.aov <- aov(fact_t2~group*time+Error(id),data=data2)
summary(data1.aov)

data3.1 <- subset(data2,group==1 | group==2)


t.test(fact_t2~group,data3.1[data3.1$time==1,])
t.test(fact_t2~group,data3.1[data3.1$time==2,])
t.test(fact_t2~group,data3.1[data3.1$time==3,])
t.test(fact_t2~group,data3.1[data3.1$time==4,])




####Q3


data7.1 <- subset(data2,time==1 | time==2)
data7.2 <- subset(data2,time==1 | time==3)
data7.3 <- subset(data2,time==1 | time==4)
data7.4 <- subset(data2,time==2 | time==3)
data7.5 <- subset(data2,time==2 | time==4)
data7.6 <- subset(data2,time==3 | time==4)

t.test(fact_t2~time,data7.1[data7.1$group==1,])
t.test(fact_t2~time,data7.2[data7.2$group==1,])
t.test(fact_t2~time,data7.3[data7.3$group==1,])
t.test(fact_t2~time,data7.4[data7.4$group==1,])
t.test(fact_t2~time,data7.5[data7.5$group==1,])
t.test(fact_t2~time,data7.6[data7.6$group==1,])

t.test(fact_t2~time,data7.1[data7.1$group==2,])
t.test(fact_t2~time,data7.2[data7.2$group==2,])
t.test(fact_t2~time,data7.3[data7.3$group==2,])
t.test(fact_t2~time,data7.4[data7.4$group==2,])
t.test(fact_t2~time,data7.5[data7.5$group==2,])
t.test(fact_t2~time,data7.6[data7.6$group==2,])






library(nlme)
longg <- groupedData( fact_t2~ as.numeric(group) * as.numeric(time) | id, data = data2)
summary(longg)
fit.cs <- gls( fact_t2~ group * time, data = longg,
  corr = corCompSymm(, form= ~ 1 | id) )
summary(fit.cs)
intervals(fit.cs)
anova(fit.cs)
confint(fit.cs)

fit.un <- gls(fact_t2~ group * time, data = longg,
  corr=corSymm(form = ~ 1 | id),
  weights = varIdent(form = ~ 1 | time))
summary(fit.un)
intervals(fit.un)
anova(fit.un)
confint(fit.un)


fit.ar1 <- gls(fact_t2~ group * time, data = longg,
  corr = corAR1(, form= ~ 1 | id))
summary(fit.ar1)
intervals(fit.ar1)
anova(fit.ar1)
confint(fit.ar1)



fit.arh1 <- gls(fact_t2~ group * time, data = longg,
  corr = corAR1(, form = ~ 1 | id), weight = varIdent(form = ~ 1 | time))
summary(fit.arh1)
intervals(fit.arh1)
anova(fit.arh1)
confint(fit.arh1)



anova(fit.cs, fit.un)
anova(fit.cs, fit.ar1)
anova(fit.cs, fit.arh1)
anova(fit.un, fit.arh1)

require(lattice)
par(cex=0.6)
xyplot(fact_t2~time,data=data2,groups=group,type="o",panel=panel.superpose)
xyplot( fact_t2~ time |group, data = data2, groups = id,type = "o", panel = panel.superpose)



time.linear <- lme(fact_t2~ group * time,random = list(id = pdDiag(~ time)), data = data2)
summary(time.linear)
anova(time.linear)

fitted <- fitted(time.linear, level=0)

with(data2, plot(time[group==3], fitted[group==3], ylim = c(50, 150),
  xlab = "time", ylab = "predicted", type = "b", col = "green"))
with(data2, points(time[group==2], fitted[group==2],
  pch = 4, type = "b", col = "red"))
with(data2, points(time[group==1], fitted[group==1],
  pch = 16, type = "b", col = "blue"))

###############################################################


require(lme4)
lme1 <- lmer(fact_t2~group*time+(1|id),data=data2,REML=F)
summary(lme1)
dotplot(ranef(lme1,which="id",postVar=T),scales=list(y=list(alternating=0)))
coef(lme1)
fixef(lme1)
resid <- residuals(lme1)
qqnorm(resid)
qqline(resid)
require(influence.ME)
se.fixef(lme1)
alt.est <- influence(lme1,group="id",gf="all")
pchange(alt.est)
cooks.distance(alt.est)
dfbetas(alt.est)
data4 <- within(data1,{
     group <- as.factor(exptx)
     time <- as.factor(funo)
     })
lme2 <- lmer(fact_t2~group*time+(1|id),data=data2,REML=F)


model.b <- exclude.influence(lme2, grouping="id",level=c("1","2","3","4","5"))
summary(model.b)
grouping.levels(lme1, "id")
require(LMERConvenienceFunctions)
#Remove the outliers
romr.fnc(lme1,data=data2,trim=2.5)$data0
romr.fnc(lme1,data=data2,trim=2.5)$data

plot(alt.est,which="cook",sort=T)
plot(alt.est,which="dfbetas")
sigtest(alt.est,test=1.96)$structure

alt.est$alt.fixed
summary(alt.est$alt.fixed)

## see ?"profile-methods"
mySumm <- function(.) { s <- sigma(.)
c(beta =getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta"))) }
(t0 <- mySumm(fm01ML)) # just three parameters
## alternatively:
mySumm2 <- function(.) {
c(beta=fixef(.),sigma=sigma(.), sig01=sqrt(unlist(VarCorr(.))))
}
set.seed(101)
## 3.8s (on a 5600 MIPS 64bit fast(year 2009) desktop "AMD Phenom(tm) II X4 925"):
system.time( boo01 <- bootMer(lme1, mySumm2, nsim = 100) )
## to "look" at it
require("boot") ## a recommended package, i.e. *must* be there
boo01
## note large estimated bias for sig01
## (~30% low, decreases _slightly_ for nsim = 1000)
## extract the bootstrapped values as a data frame ...
head(as.data.frame(boo01))
## ------ Bootstrap-based confidence intervals ------------
## intercept
(bCI.1 <- boot.ci(boo01, index=1, type=c("norm", "basic", "perc")))# beta

## Residual standard deviation - original scale:
(bCI.2 <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc")))
## Residual SD - transform to log scale:
(bCI.2l <- boot.ci(boo01, index=2, type=c("norm", "basic", "perc"),
h = log, hdot = function(.) 1/., hinv = exp))
## Among-batch variance:
(bCI.3 <- boot.ci(boo01, index=3, type=c("norm", "basic", "perc")))# sig01
## Graphical examination:
plot(boo01,index=3)
## Check stored values from a longer (1000-replicate) run:
load(system.file("testdata","boo01L.RData",package="lme4"))
plot(boo01L,index=3)
confint(lme1, level = 0.95,
method = "boot",
nsim = 500,
boot.type = "perc", quiet = FALSE,
)



## likelihood ratio tests
drop1(lme1,test="Chisq")
## use Kenward-Roger corrected F test, or parametric bootstrap,
## to test the significance of each dropped predictor
install.packages("pbkrtest")
if (require(pbkrtest) && packageVersion("pbkrtest")>="0.3.8") {
KRSumFun <- function(object, objectDrop, ...) {
krnames <- c("ndf","ddf","Fstat","p.value","F.scaling")
r <- if (missing(objectDrop)) {
setNames(rep(NA,length(krnames)),krnames)
} else {
krtest <- KRmodcomp(object,objectDrop)
unlist(krtest$stats[krnames])
}
attr(r,"method") <- c("Kenward-Roger via pbkrtest package")
r
}
drop1(lme1,test="user",sumFun=KRSumFun)
if(lme4:::testLevel() >= 3) { ## takes about 16 sec
nsim <- 100
PBSumFun <- function(object, objectDrop, ...) {
pbnames <- c("stat","p.value")
r <- if (missing(objectDrop)) {
setNames(rep(NA,length(pbnames)),pbnames)
} else {
pbtest <- PBmodcomp(object,objectDrop,nsim=nsim)
unlist(pbtest$test[2,pbnames])
}
attr(r,"method") <- c("Parametric bootstrap via pbkrtest package")
r
}
system.time(drop1(lme1,test="user",sumFun=PBSumFun))
}
}
## workaround for creating a formula in a separate environment
createFormula <- function(resp, fixed, rand) {
f <- reformulate(c(fixed,rand),response=resp)
## use the parent (createModel) environment, not the
## environment of this function (which does not contain 'data')
environment(f) <- parent.frame()
f
}
createModel <- function(data) {
mf.final <- createFormula("fact_t2", "group","time", "(group|id)")
lmer(mf.final, data=data)
}
drop1(createModel(data=data2))

f <- fact_t2~group*time+(1||id)

fortify.merMod(lme1, data =data2)
library(HLMdiag)
stdresid    <- HLMresid(lme1, level = 1, standardize = TRUE)






se <- sqrt(diag(vcov(lme1)))
(tab <- cbind(Est=fixef(lme1),LL=fixef(lme1)-1.96*se,UL=fixef(lme1)+1.96*se))
sampler <- function(dat, clustervar, replace = TRUE, reps = 1) {
    cid <- unique(dat[, clustervar[1]])
    ncid <- length(cid)
    recid <- sample(cid, size = ncid * reps, replace = TRUE)
    if (replace) {
        rid <- lapply(seq_along(recid), function(i) {
            cbind(NewID = i, RowID = sample(which(dat[, clustervar] == recid[i]),
                size = length(which(dat[, clustervar] == recid[i])), replace = TRUE))
        })
    } else {
        rid <- lapply(seq_along(recid), function(i) {
            cbind(NewID = i, RowID = which(dat[, clustervar] == recid[i]))
        })
    }
    dat <- as.data.frame(do.call(rbind, rid))
    dat$Replicate <- factor(cut(dat$NewID, breaks = c(1, ncid * 1:reps), include.lowest = TRUE,
        labels = FALSE))
    dat$NewID <- factor(dat$NewID)
    return(dat)
}
set.seed(20)
tmp <- sampler(data2, "id", reps = 100)
bigdata <- cbind(tmp, data2[tmp$RowID, ])
r <- getME(lme1,"theta")
f <- fixef(lme1)
require(snow)
cl <- makeCluster(4)
clusterExport(cl, c("bigdata", "f", "r"))
clusterEvalQ(cl, require(lme4))
myboot <- function(i) {
    object <- try(lmer(fact_t2~group*time+(1|id),data=data2, subset = Replicate == i,
        nAGQ = 1, start = list(fixef = f, theta = r)), silent = TRUE)
    if (class(object) == "try-error")
        return(object)
    c(fixef(object), getME(object, "theta"))
}
start <- proc.time()
res <- parLapplyLB(cl, X = levels(bigdata$Replicate), fun = myboot)
end <- proc.time()
stopCluster(cl)
success <- sapply(res, is.numeric)
mean(success)
bigres <- do.call(cbind, res[success])
# calculate 2.5th and 97.5th percentiles for 95% CI
(ci <- t(apply(bigres, 1, quantile, probs = c(0.025, 0.975))))
# All results
finaltable <- cbind(Est = c(f, r), SE = c(se, NA), BootMean = rowMeans(bigres),
    ci)
# round and print
round(finaltable, 3)






############################################################






############################
######Sensitivity Analysis######
############################
####Missing values



help.data1 <- mydata[,c("sex","wt_loss","sx_pri","sx_met","sx_sys","sx_cd","p_rt","crpr","pd_lt6","death","trtment","exptx","qol_comp")]
help.data2 <- subset(help.data1,select=-c(sex,wt_loss,sx_pri,sx_met,sx_sys,sx_cd,p_rt,crpr,pd_lt6,death,trtment,exptx,qol_comp))
help.data3 <- subset(mydata,select=-c(sex,wt_loss,sx_pri,sx_met,sx_sys,sx_cd,p_rt,crpr,pd_lt6,death,trtment,exptx,qol_comp))
attach(help.data1)
sex<- as.factor(sex)
wt_loss<- as.factor(wt_loss)
sx_pri <- as.factor(sx_pri)
sx_met <- as.factor(sx_met)
sx_sys <- as.factor(sx_sys)
sx_cd <- as.factor(sx_cd)
p_rt <- as.factor(p_rt)
crpr <- as.factor(crpr)
pd_lt6 <- as.factor(pd_lt6)
death <- as.factor(death)
trtment <- as.factor(trtment)
exptx <- as.factor(exptx)
qol_comp <- as.factor(qol_comp)
detach(help.data1)
help.data1 <- data.frame(cbind(help.data2,sex,wt_loss,sx_pri,sx_met,sx_sys,sx_cd,p_rt,crpr,pd_lt6,death,trtment,exptx,qol_comp))
data1 <- cbind(help.data1,help.data3)




require(mice)
missing.pattern <- md.pattern(data1)
imp1 <-mice(data1,m=5,maxit=10,seed=23109,
           method=c("logreg","polyreg",rep("logreg",8),"polyreg","logreg","polyreg",rep("pmm",19)))
summary(imp1)
#Geting the complete data frame, there is no missing data any more.
complete.data1 <- complete(imp1)
stripplot(imp1,pch=20,cex=1.2)
imp1$group <- as.factor(imp1$trtment)
imp1$time <- as.factor(imp1$funo)

data2 <- within(complete.data1,{
     group <- as.factor(trtment)
     time <- as.factor(funo)
     id <- as.factor(patid)
     })
par(cex=0.6)
with(data2,interaction.plot(time,group,fact_t2,ylim=c(7,100),lty=c(1,2,3,4),lwd=3,col=c("chartreuse","blue","brown"),trace.label="group"))



require(lme4)
glm1 <- lmer(fact_t2~time*group+(1|id),data=data2)

###Q1
require(doBy)
summary(fact_t2~time,data=data2,FUN=list(mean,sd))

###Q2
require(car)
data1.aov <- aov(fact_t2~group*time+Error(id),data=data2)
summary(data1.aov)

data3.1 <- subset(data2,group==1 | group==2)
data3.2 <- subset(data2,group==1 | group==3)

t.test(fact_t2~group,data3.1[data3.1$time==1,])
t.test(fact_t2~group,data3.1[data3.1$time==2,])
t.test(fact_t2~group,data3.1[data3.1$time==3,])
t.test(fact_t2~group,data3.1[data3.1$time==4,])

t.test(fact_t2~group,data3.2[data3.2$time==1,])
t.test(fact_t2~group,data3.2[data3.2$time==2,])
t.test(fact_t2~group,data3.2[data3.2$time==3,])
t.test(fact_t2~group,data3.2[data3.2$time==4,])


####Q3


data7.1 <- subset(data2,time==1 | time==2)
data7.2 <- subset(data2,time==1 | time==3)
data7.3 <- subset(data2,time==1 | time==4)
data7.4 <- subset(data2,time==2 | time==3)
data7.5 <- subset(data2,time==2 | time==4)
data7.6 <- subset(data2,time==3 | time==4)

t.test(fact_t2~time,data7.1[data7.1$group==1,])
t.test(fact_t2~time,data7.2[data7.2$group==1,])
t.test(fact_t2~time,data7.3[data7.3$group==1,])
t.test(fact_t2~time,data7.4[data7.4$group==1,])
t.test(fact_t2~time,data7.5[data7.5$group==1,])
t.test(fact_t2~time,data7.6[data7.6$group==1,])

t.test(fact_t2~time,data7.1[data7.1$group==2,])
t.test(fact_t2~time,data7.2[data7.2$group==2,])
t.test(fact_t2~time,data7.3[data7.3$group==2,])
t.test(fact_t2~time,data7.4[data7.4$group==2,])
t.test(fact_t2~time,data7.5[data7.5$group==2,])
t.test(fact_t2~time,data7.6[data7.6$group==2,])

t.test(fact_t2~time,data7.1[data7.1$group==3,])
t.test(fact_t2~time,data7.2[data7.2$group==3,])
t.test(fact_t2~time,data7.3[data7.3$group==3,])
t.test(fact_t2~time,data7.4[data7.4$group==3,])
t.test(fact_t2~time,data7.5[data7.5$group==3,])
t.test(fact_t2~time,data7.6[data7.6$group==3,])



require(lme4)
lme1 <- lmer(fact_t2~group*time+(1|id),data=data2)
summary(lme1)



###Two Factor Design with Repeated Measures on One Factor
require(agricolae)
(with(data2, HSD.test(fact_t2, group, DFerror = 8, MSerror = 229210)))
(with(data2, HSD.test(fact_t2, time, DFerror = 16, MSerror = 358)))
group1 <- data2[data2$group ==1, ]
group2 <- data2[data2$group ==2, ]
group3 <- data2[data2$group ==3, ]

(with(group1, HSD.test(fact_t2, time, DFerror = 24, MSerror = 1.95, 
    alpha = 0.05/2)))
(with(group2, HSD.test(fact_t2, time, DFerror = 24, MSerror = 1.95, 
    alpha = 0.05/2)))
(with(group3, HSD.test(fact_t2, time, DFerror = 24, MSerror = 1.95, 
    alpha = 0.05/2)))


with(data2, tapply(fact_t2, list(time,group), mean))
with(data2, boxplot(fact_t2~ time + group))    # output not shown
with(data2, boxplot(fact_t2 ~ group + time))    # compare this one with the last one
title(main="Boxplot of time and group")
title(ylab="Boxplot of group and time")


p <- ggplot(data2, aes(time, fact_t2))
p + geom_boxplot(aes(fill = group))
                 


############################################################
lmeFit <- lme( fact_t2~ time, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$group==1,])
anova(lmeFit)
anova(lme(fact_t2 ~ time, random=list(id=pdCompSymm(~time-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ time + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(time="Tukey"))
summary(contr)
confint(contr)

lmeFit <- lme( fact_t2~ time, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$group==2,])
anova(lmeFit)
anova(lme(fact_t2 ~ time, random=list(id=pdCompSymm(~time-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ time + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(time="Tukey"))
summary(contr)
confint(contr)


lmeFit <- lme( fact_t2~ time, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$group==3,])
anova(lmeFit)
anova(lme(fact_t2 ~ time, random=list(id=pdCompSymm(~time-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ time + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(time="Tukey"))
summary(contr)
confint(contr)






lmeFit <- lme( fact_t2~ group, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$time==1,])
anova(lmeFit)
anova(lme(fact_t2 ~ group, random=list(id=pdCompSymm(~group-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ group + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(group="Tukey"))
summary(contr)
confint(contr)

lmeFit <- lme( fact_t2~ group, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$time==2,])
anova(lmeFit)
anova(lme(fact_t2 ~ group, random=list(id=pdCompSymm(~group-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ group + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(group="Tukey"))
summary(contr)
confint(contr)

lmeFit <- lme( fact_t2~ group, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$time==3,])
anova(lmeFit)
anova(lme(fact_t2 ~ group, random=list(id=pdCompSymm(~group-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ group + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(group="Tukey"))
summary(contr)
confint(contr)


lmeFit <- lme( fact_t2~ group, random=~1 | id, correlation=corCompSymm(form=~1|id),
              method="ML", data=data2[data2$time==4,])
anova(lmeFit)
anova(lme(fact_t2 ~ group, random=list(id=pdCompSymm(~group-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ group + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(group="Tukey"))
summary(contr)
confint(contr)

lmeFit <- lme( fact_t2~ group*time, random=~1 | id, correlation=corCompSymm(form=~1|id), method="ML", data=data2)
anova(lmeFit)
anova(lme(fact_t2 ~ group*time, random=list(id=pdCompSymm(~group-1)), method="REML", data=data2))
library(lme4)
anova(lmer(fact_t2 ~ group*time + (1|id), data=data2))
library(multcomp)
contr <- glht(lmeFit, linfct=mcp(group="Tukey"))
summary(contr)
confint(contr)
contr <- glht(lmeFit, linfct=mcp(time="Tukey"))
summary(contr)
confint(contr)

#############################################################
#############################################################
############################################################
############################################################
###########################################################
###########################################################
###########################################################


source("http://www.r-statistics.com/wp-content/uploads/2010/02/Friedman-Test-with-Post-Hoc.r.txt")  # loading the friedman.test.with.post.hoc function from the internet
 
	### Comparison of three Wine ("Wine A", "Wine B", and
	###  "Wine C") for rounding first base. 
	WineTasting <- data.frame(
		  Taste = c(5.40, 5.50, 5.55,
					5.85, 5.70, 5.75,
					5.20, 5.60, 5.50,
					5.55, 5.50, 5.40,
					5.90, 5.85, 5.70,
					5.45, 5.55, 5.60,
					5.40, 5.40, 5.35,
					5.45, 5.50, 5.35,
					5.25, 5.15, 5.00,
					5.85, 5.80, 5.70,
					5.25, 5.20, 5.10,
					5.65, 5.55, 5.45,
					5.60, 5.35, 5.45,
					5.05, 5.00, 4.95,
					5.50, 5.50, 5.40,
					5.45, 5.55, 5.50,
					5.55, 5.55, 5.35,
					5.45, 5.50, 5.55,
					5.50, 5.45, 5.25,
					5.65, 5.60, 5.40,
					5.70, 5.65, 5.55,
					6.30, 6.30, 6.25),
					Wine = factor(rep(c("Wine A", "Wine B", "Wine C"), 22)),
					Taster = factor(rep(1:22, rep(3, 22))))
 
	with(WineTasting , boxplot( Taste  ~ Wine )) # boxploting 
	friedman.test.with.post.hoc(Taste ~ Wine | Taster ,WineTasting)	# the same with our function. With post hoc, and cool plots



friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, to.post.hoc.if.signif = T,  to.plot.parallel = T, to.plot.boxplot = T, signif.P = .05, color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
	# formu is a formula of the shape: 	Y ~ X | block
	# data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
 
	# Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
 
 
	# Loading needed packages
	if(!require(coin))
	{
		print("You are missing the package 'coin', we will now try to install it...")
		install.packages("coin")		
		library(coin)
	}
 
	if(!require(multcomp))
	{
		print("You are missing the package 'multcomp', we will now try to install it...")
		install.packages("multcomp")
		library(multcomp)
	}
 
	if(!require(colorspace))
	{
		print("You are missing the package 'colorspace', we will now try to install it...")
		install.packages("colorspace")
		library(colorspace)
	}
 
 
	# get the names out of the formula
	formu.names <- all.vars(formu)
	Y.name <- formu.names[1]
	X.name <- formu.names[2]
	block.name <- formu.names[3]
 
	if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
 
	# Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
 
	# stopping in case there is NA in the Y vector
	if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
 
	# make sure that the number of factors goes with the actual values present in the data:
	data[,X.name ] <- factor(data[,X.name ])
	data[,block.name ] <- factor(data[,block.name ])
	number.of.X.levels <- length(levels(data[,X.name ]))
	if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
 
	# making the object that will hold the friedman test and the other.
	the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons	
						   teststat = "max",
						   xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
						   ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
						)
	# if(to.print.friedman) { print(the.sym.test) }
 
 
	if(to.post.hoc.if.signif)
		{
			if(pvalue(the.sym.test) < signif.P)
			{
				# the post hoc test
				The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
 
 
				# plotting
				if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
 
				if(to.plot.parallel)
				{
					X.names <- levels(data[, X.name])
					X.for.plot <- seq_along(X.names)
					plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
 
					if(color.blocks.in.cor.plot) 
					{
						blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
					} else {
						blocks.col <- 1 # black
					}					
 
					data2 <- data
					if(jitter.Y.in.cor.plot) {
						data2[,Y.name] <- jitter(data2[,Y.name])
						par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"				
					} else {
						par.cor.plot.text <- "Parallel coordinates plot"
					}				
 
					# adding a Parallel coordinates plot
					matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
									 direction="wide")[,-1])  , 
							type = "l",  lty = 1, axes = FALSE, ylab = Y.name, 
							xlim = plot.xlim,
							col = blocks.col,
							main = par.cor.plot.text)
					axis(1, at = X.for.plot , labels = X.names) # plot X axis
					axis(2) # plot Y axis
					points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
				}
 
				if(to.plot.boxplot)
				{
					# first we create a function to create a new Y, by substracting different combinations of X levels from each other.
					subtract.a.from.b <- function(a.b , the.data)
					{
						the.data[,a.b[2]] - the.data[,a.b[1]]
					}
 
					temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
									 direction="wide") 	#[,-1]
					wide.data <- as.matrix(t(temp.wide[,-1]))
					colnames(wide.data) <- temp.wide[,1]
 
					Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
					names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
 
					the.ylim <- range(Y.b.minus.a.combos)
					the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
					is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
 
					boxplot(Y.b.minus.a.combos,
						names = names.b.minus.a.combos ,
						col = is.signif.color,
						main = "Boxplots (of the differences)",
						ylim = the.ylim
						)
					legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
					abline(h = 0, col = "blue")
 
				}
 
				list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
				if(to.print.friedman) {print(list.to.return)}				
				return(list.to.return)
 
			}	else {
					print("The results where not significant, There is no need for a post hoc test")
					return(the.sym.test)
				}					
	}
 
# Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
# http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}




bwplot(fact_t2 ~ time,data2, groups = group,
       panel = "panel.superpose",
       panel.groups = "panel.linejoin",
       xlab = "time",
       key = list(lines = Rows(trellis.par.get("superpose.line"),
                  c(1:3, 1)),
                  text = list(lab = as.character(unique(data2$group))),
                  columns = 4, title = "Row position"))
xyplot(fact_t2 ~ time,data2, groups = group,
       type = "a",
       auto.key =
       list(space = "right", points = FALSE, lines = TRUE))



xyplot(fact_t2~time,data2,
       panel=function(x,y,...)
       {
           for (ii in unique(data2$id))
               {pdata=subset(data2,id==ii)
                 colg=unique(pdata$group)
           panel.xyplot(sort(pdata$time),pdata$fact_t2[order(pdata$time)],type="l",col=colg)
       }
       }
)










library(ggplot2)
seed(1234)
dat <- data.frame(Year=rep(2000:2013,5),
            value=rep(1:5,each=14)+rnorm(5*14,0,.5),
            Name=rep(c("Name1","End","First","Name2","Name 3"),each=14))
dat2 <- dat
dat2$value[sample.int(5*14,12)]=NA
#dat3 is probably the example of what your data looks like except that I'm treating Year as an integer.

dat3 <- dat2[!is.na(dat2$value),]


# POINTS ARE CONNECTED WITH NO DATA IN BETWEEN #
ggplot(dat3, aes(Year, value, colour=Name)) + 
     geom_line() + geom_point()
#However if you add columns in your data for the years that are missing a column and setting that value to NA then when you plot the data you'll get the gaps.

# POINTS ARE NOT CONNECTED 
ggplot(dat2, aes(Year, value, colour=Name)) + 
     geom_line() + geom_point()
And finally, to answer your last question this is how you change the order and labels of Name in the legend:

# CHANGE THE ORDER AND LABELS IN THE LEGEND #
ggplot(dat2, aes(Year, value, colour=Name)) + 
     geom_line() + geom_point() + 
     scale_colour_discrete(labels=c("Beginning","Name 1","Name 2","Name 3","End"),
                             breaks=c("First","Name1","Name2","Name 3","End")))







library(lattice)
## the data
 junk = data.frame(
  Visit = as.factor(rep(seq(1,5), 2)),
  Drug = rep(c("D", "P"), each = 5),
  Aldo = c(13, NA, NA, 15, 14, 12, NA, NA, 14, 13),
  SE.Aldo = c(3,  NA, NA, 3, 3, 2, NA, NA, 2, 2),
  lower.ci.Aldo = c(10, NA, NA, 12, 11,  10,NA,  NA, 12, 11),
  upper.ci.Aldo = c(16, NA, NA, 18, 17, 14, NA, NA, 16, 15)
  )

  ## functions for the error bars
  prepanel.ci <- function(x, y, ly, uy, subscripts, ...) {
     x <- as.numeric(x)
     ly <- as.numeric(ly[subscripts])
     uy <- as.numeric(uy[subscripts])
     list(ylim = range(y, uy, ly, finite = TRUE)) }
  panel.ci <- function(x, y, ly, uy, subscripts, pch = 16, ...) {
     x <- as.numeric(x)
     y <- as.numeric(y)
     ly <- as.numeric(ly[subscripts])
     uy <- as.numeric(uy[subscripts])
     panel.arrows(x, ly, x, uy, col = "black",
                  length = 0.25, unit = "native",
                  angle = 90, code = 3)
     panel.xyplot(x, y, pch = 16, ...)}


  ## the plot, but visit 1 not connected to visit 4
  xyplot(Aldo ~ as.numeric(Visit), xlab="Visit", ylab="Aldo",
         groups=Drug,
         data=junk,
         ly = junk$lower.ci.Aldo,
         uy = junk$upper.ci.Aldo,
         prepanel = prepanel.ci,
         panel = panel.superpose,
         panel.groups = panel.ci,
         type="b",
         auto.key = list(space = "top",  text = c( "D","P"), points = FALSE,
  lines = TRUE, columns=2),
  par.settings = list(superpose.line = list(lty = c(1,5), col=c('black',
  'black') ) )        )


##################################################################
##################################################################
##################################################################
##################################################################
 ## deal with NAs:
     esoph[66,] # second to last age group: 65-74
     esophNA <- esoph; esophNA$ncases[66] <- NA
     with(esophNA, {
       interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5)
                                     # doesn't show *last* group either
       interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5, type = "b")})
       ## alternative take non-NA's  {"cheating"}
       interaction.plot(agegp, alcgp, ncases/ncontrols, col = 2:5,
                        fun = function(x) mean(x, na.rm = TRUE),
                        sub = "function(x) mean(x, na.rm=TRUE)")
     })
     rm(esophNA) # to clear up
