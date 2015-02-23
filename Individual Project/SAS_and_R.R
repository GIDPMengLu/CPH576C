#Default names from read.table()denoted V1 through V4

tmpds <- read.table("")
id <- tmpds$V1
initials <- tmpds$V2
datevar <- as.Date(as.character(tmpds$V3),"%m%d%y")
cost <- as.numeric(substr(tmpds$V4,2,100))
ds <- data.frame(id,initials, datevar,cost)
rm(tmpds,id,initials,datevar,cost)

#########################################################
#Reading datasets in other formats
require(foreign)
ds <- read.dbf("filename.dbf")               #DBase
ds <- read.epiinfo("filename.epiinfo")    #Epi Info
ds <- read.mtp("filename.mtp")             #Minitab protable worksheet
ds <- read.octave("filename.octave")     #Octave
ds <- read.ssd("filename.ssd")               #SAS version 6
ds <- read.xport("filename.xport")         #SAS XPORT file
ds <- read.spss("filename.sav")             #SPSS
ds <- read.dta("filename.dta")               #Stata
ds <- read.systat("filename.sys")           #Systat


###################################################
#URL
#The readLines() function reads arbitrary text, while read.table() can be used to read a file with cases corresponding to lines and variables to fields in the file(the header option sets variable names to entires in the first line). The read.csv() function can be used to read comma separated values. Access through proxy servers as well as specification of username and passwords is provided by the function download.file(). A limit on the number of lines to be read can be specified through the nrows option.
urlhandle <- url("http://www.math.smith.edu/sasr/testdata")
ds <- readLines(urlhandle)

ds <- read.table("http://www.math.smith.edu/sasr/testdata")
ds <- read.csv("http://www.math.smith.edu/sasr/file.csv")

###############################################
#XML(extensible markup language)
#The xmlRoot() function opens a connection to the file, while xmlSApply() and xmlValue() are called recursively to process the file. The returned object is a character matrix with columns corresponding to observations and rows corresponding to variables.
require(XML)
urlstring <- "http://www.math.smith.edu/sasr/datasets/help.xml"
doc <- xmlRoot(xmlTreeParse(urlstring))
tmp <- xmlSApply(doc,function(x) xmlSApply(x,xmlValue))
ds <- t(tmp)[,-1]


###################################################
#Data entry
#Create an empty data set
#The data.entry() function differs from the edit() function, which leaves the objects given as argument unchanged, returning a new object with the desired edits
x <- numeric(10)
data.entry(x)


#######################################################
#Save a native dataset
save(robject,file="savedfile")

#######
#Creating files for use by other packages
require("foreign")
write.dta(ds,"filename.dta")
write.dbf(ds,"filename.dbf")
write.foreign(ds,"filename.dat","filename.sas",package="SAS")


################################################
#Creating datasets in text format
#The sep option to write.table() can be used to charge the default delimiter(space) to an arbitrary value
require(foreign)
write.csv(ds,file="full_file_location_and_name")
write.table(sa,file="full_file_location_and_name")


####################################
#Number of digits to display
options(digits=n)


###################################################
#Creating HTML formatted output
#The cat() function is used inside the script file to generate output.
install.packages("prettyR")
require(prettyR)
htmlize("script.R",title="mytitle",echo=TRUE)


###################################################
#Creating XML datasets and output




####################################################
#Names of variables and their types
#sapply(ds,class)will return the names and classes(e.g., numeric integer or character) of each variable within a dataframe, while running summary(ds) will provide an overview of the distribution of each column.
str(ds)
sapply(ds,class)




#####################################################
#Values of variables in a dataset
print(ds)
View(ds)
edit(ds)



#####################################################
#Rename variables in a dataset
names(ds)[names(ds)=="old1"] <- "new1"
names(ds)[names(ds)=="old2"] <- "new2"
ds <- within(ds,{new1 <- old1;new2 <- old2;rm(old1,old2)})
require(reshape)
ds <- rename(ds,c("old1"="new1","old2"="new2"))


########################################################
#Add comment to a dataset or variable
#The attributes() function can be used to list all attributes, including any comment(), while the comment() function without an argument on the right hand side will display the comment, if present.
comment(ds) <- "This is a comment about the dataset"


#########################################################
###Derived variables and data manipulation
##Creating string variables from numeric variables
stringx <- as.character(numericx)
typeof(stringx)  #The typeof() function can be used to verify the type of an object; possible values include logical, integer, double, complex, character, raw, NULL, closure(function), special and builtin
typeof(numericx)

##Creating numeric variables from string variables

numericx <- as.numeric(stringx)
typeof(stringx)
typeof(numericx)


##########################################################
#Devived variables and data manipulation
##length of strinf variables
len <- nchar(stringx)
#The nchar() function returns a vector of lengths of each of the elements of the string vector given as argument, as opposed to the length() function returns the number of elements in a vector.



################################################
#Find strings within string variables
#The function grep() returns a list of elements in the vector given by stringx that match the given pattern, while the regexpr() function returns a numeric list if starting points in each string in the list(with -1 if there was no match).
matches <- grep("pat",stringx)
positions <- regexpr("pat",stringx)
x <- c("abc","def","abcdef","defabc")
grep("abc",x)
regexpr("abc",x)
attr(,"match.length")
regexpr("abc",x)<0



####################################################
#Creating categorical variables from continuous variables
newcat1 <- (x>=minval)+(x>=cutpoint1)+...+(x>=cutpointn)


##Recode a categorical variables
tmpcat <- oldcat
tmpcat[oldcat==val1] <- newval1
tmpcat[oldcat==val2] <- newval2
...
tmpcat[oldcat==valn] <- newvaln
newcat <- as.factor(tmpcat)
##OR
newcat <- cut(x,breaks=c(val2,...,valn),labels=c("cut1","cut2",...,"cutn"),right=FALSE)
#Using logistic
tmpvar <- rep(NA,length(oldvar))
tmp[oldvar==0] <- 0
tmp[oldvar>0 & oldvar<50 & group=="A"] <- 1
tmp[oldvar>0 & oldvar<60 & group=="B"] <- 1
tmp[oldvar>=50 & group=="A"] <- 2
tmp[oldvar>=60 & group=="B"] <- 2
newvar <- as.factor(tmpvar)


##############################################
##Formatting values of variables
x<-c(0,0,1,1,2)
x <- factor(x,0:2,labels=c("Control","Low Dose","High Dose"))





#####################################################
##Account for missing values
mean(c(1,2,NA))
mean(c(1,2,NA),na.rm=TRUE)
sum(na.omit(c(1,2,NA)))
x <- c(1,3,NA)
sum(!is.na(x))#counting
mean(x)
mean(x,na.rm=TRUE)
#Remap values of x with missing value code of 999 to missing
x[x===999] <- NA
#Set 999's to missing
is.na(x) <- x==999
#Returns a vector of logisticals
is.na(x)
#removes observations that are missing on that variable
na.omit(x)
#removes observations that are missing any variable
na.omit(ds)
require(Hmisc)
#display patterns of missing variables in a dataframe
na.pattern(ds)


#Unique values
#The unique() function returns each of the unique values represented by the vector or dataframe denoted by x
uniquevalues <- unique(x)
uniquevalues <- unique(data.frame(x1,...,xk))



##############################################################
#Random sample of a dataset
#Permutation of a variable
newx <- sample(x,replace=FALSE)
#Permutation of a dataset
obs <- sample(1:dim(ds)[1],dim(ds)[1],replace=FALSE)
newds <- ds[obs,]


############################################################
#Convert from wide to long (tall) format
#longitudinal measures
long <- reshape(wide,idvar="id",varying=list(names(wide)[3:5]),v.names="inc",timevar="year",times=80:82,direction="long")

#Convert from long(tall) to wide format
wide <- reshape(long,v.names="inc",idvar="id",tmevar="year",direction="wide")



############################################################
#Sort datasets
sortds <- ds[order(x1,x2,...,xk)]
library(plyr)
arrange(ds,desc(x1,x2,...,xk),b)


###########################################################
#merge datasets
newds <- merge(ds1,ds2,by=id,all=TRUE)
###The all option specifies that extra rows will be added to the output for any rows that have no matches in the other dataset.



########################################################
#List and access files
list.files()
#The list.files() command returns a character vector of filenames in the current directory(by default)
#The function file.choose() provides an interactive file brower, and can be given as an argument to functions such as read.table() or read.csv(). Related file operation functions include file.access(), file.info() and files()




#########################################################
#
require(gtools)
nchooser <- length(combinations(n,r)[,1])
npermr <- length(permutatios(n,r)[,1])
nextintx <- ceiling(x)
justintx <- floor(x)
round2dec <- round(x,2)
roundint <- round(x)
keep4sig <- signif(x,4)
movetozero <- trunc(x)


###########################################################
#Derivative
D(expression(x^3),"x")
#deriv() are useful in numerical optimization



########################################################
#Optimization problems
f <- function(x){return(x*(10-x))}
optimize(f,interval=c(0,10),maximum=TRUE)
#nlm()
#optim()
#constroptim()


########################################################
#Calculate singular value decomposition
a<-matrix(c(1,2,3,4),2,2,byrow=T)
svd<-svd(a)
U<- svd$u
Q <- svd$d
V <- svd$v


#####################################################
#Distribution function
beta()
binom()
cauchy()
chisq()
exp()
f()
gamma()
geom()
hyper()
logis()
lnorm() #lognormal
nbinom() # negative binomial
norm()
pois()
t()
unif()
weibull
install.packages("VGAM")
require(VGAM)
betabin() #Beta-binomial
inv.gaussian() #inverse Normal
laplace() #Laplace
require(Hmisc)
x <- rMultinom(matrix(c(p1,p2,...,pr),1,r),n)
#The function rMultinom() from the Hmisc library allows the specification of the desireed multinomial probabilities as a 1*r matrix.
rmultinom()



#################################
#Multivariate normal random variables
require(MASS)
mu <- rep(0,3)
Sigma <- matrix(c(3,1,2,1,4,0,2,0,5),nrow=3)
xvals <- mvrnorm(1000,mu,Sigma)
apply(xvals,2,mean)


rmultnorm <- function(n,mu,vmat,tol=1e-07)
    {p <- ncol(vmat)
     if(length(mu)!=p)
         step("mu vector is the wrong length")
     if(max(abs(vmat-t(vmat)))>tol)
         step("vmat not symmetric")
     vs <- svd(vmat)
     vsqrt <- t(vs$v%*%(t(vs$u)*sqrt(vs$d)))
     ans <- matrix(rnorm(n*p),nrow=n)%*%vsqrt
     ans <- sweep(ans,2,mu,"+")
     dimnames(ans) <- list(NULL,dimnames(vmat)[[2]])
     return(ans)
    }
xvals <- rmultnorm(1000,mu,Sigma)
apply(xvals,2,mean)
#sweep() function is used to transform the iunivariate normal random variables generated by rnorm to the desired mean and covariance.


beta(x,y)
gamma(x)




#########################################################
#Comparisons of floating point variables
all.equal(x,y,tolerance=0.000001)

##################################################
#Sequence of values or patterns
seq(from=i1,to=i2,length.out=nvals)
seq(from=i1,to=i2,by=1)
seq(i1,i2)

rep(value,times=nvals)
rep(value,each=nvals)


########################################################
#Five-number summary
quantile(x)
fivenum(x)
summary(x)
#fivenum() function reports the lower and upper hinges instead of the 25th and 75th percentiles.


#########################################################
#Bootstrapping a sample statistic
require(boot)
covfun <- function(x,i){sd(x[i])/mean(x[i])}
res <- boot(x,covfun,R=10000)
summary(res)
plot(res)
quantile(res$t,c(0.025,0.975))
mean(res$t)+c(-1.96,1.96)*sd(res$t)


#########################################################
#Proportion and 95% confidence interval
binom.test(sum(x),length(x))
prop.test(sum(x),length(x))
#The binom.test() function clculates an exact Clopper-Pearson confidence interval based on the F distribution using the first argument as the number of successes and the secof argument the number of trials
#prop.test() function calculates an approximate confidence interval by inverting the score test. Both allow specification of p for the null hypothesis. The conf.level option can be used to change the default confidence level.



##########################################################
#Epidemiologic statistics
tab1 <- table(x,y)
tab1[1,1]*tab1[2,2]/(tab1[1,2]*tab1[2,1])
glm1 <- glm(y~x,family=binomial)
exp(glm$coef[2])
install.packages("epitools")
require(epitools)
oddsratio.fisher(x,y)
oddsratio.wald(x,y)
riskratio(x,y)
riskratio.wald(x,y)
#the epitab() function in epitools package provides a general interface to many epidemiologic statistics, while expand.table() can be used to create individual level data from a table of counts.

#Test characteristics
sens <- sum(D==1&T==1)/sum(D==1)
spec <- sum(D==0&T==0)/sum(D==0)
install.packages("ROCR")
require("ROCR")
pred <- prediction(T,D)
diagobj <- performance(pred,"sens","spec")
spec <- slot(diagobj,"y.values")[[1]]
sens <- slot(diagobj,"x.values")[[1]]
cut <- slot(diagobj,"alpha.values")[[1]]
diagmat <- cbind(cut,sens,spec)
head(diagmat,10)

#Correlation
pearsoncorr <- cor(x,y)
spearmancorr <- cor(x,y,method="spearman")
kendalltau <- cor(x,y,method="kendall")


#kappa(agreement)
install.packages("irr")
kappa2(data.frame(x,y))


############################################################
#Contingency tables
#Display cross-classification table

mytab <- table(y,x)
addmargins(mytab)
prop.table(mytab)
xtabs(~y+x)
install.packages("prettyR")
require("prettyR")
xtab(y~x,data=ds)


##########################################################
#Pearson chi-square statistic
chisq.test(x,y)


#Cochran-Mantel-Haenszel test
mantelhaen.test(x2,x3,x1)

#Fisher's exact test
fisher.test(y,x)
fisher.test(ymat)
#The fisher.test() function command can accept either two class verctors or a matrix with counts(here denoted by ymat). For tables with many rows and/or columns, p-values can be computed using Monte Carlo simulation using the simulate.p.value option.


#McNemar's test
mcnemar.test(y,x)
# The mcnemar.test() function command can accept either two class vectors or a matrix with counts



########################################################
#Two sample tests for continuous variables
#Student's t-test
t.test(y1,y1)
t.test(y~x)
#The first example for the t.test() command displays how it can take two vectors as arguments to compare, or in the latter example a single vector corresponding to the outcome (y), with another vectoe indicatinf group membership (x) using a formula interface. By default, the two-sample t-test uses an unequal variance assumption. The option var.equal=T can be added to specify an equal variance assumption. The command var.test() can be used to formally test equality of variances.

#Nonparametric tests
wilcox.test(y1,y2)
ks.test(y1,y2)
install.packages("coin")
require(coin)
median_test(y~x)
#The wilcox.test() function uses a continuity correction in the normal approximation for the p-value. The ks.test() function does not calculate an exact p-value when there are ties. The median test shown will generate an exact p-value with the distribution="exact" option.


#Permutation test
require(coin)
oneway_test(y~as.factor(x),distribution=approximate(B=bnum))
#The oneway test function in the coin library implements a variety of permutation based tests. The distribution=approximate syntax generates an empirical p-value based on bnum Monte Carlo replicates.


#Logrank test
install.packages("survival")
require(survival)
survdiff(Surv(timevar,cens)~x)



############################################################
#Model fitting
x.factor <- as.factor(x)
mod1 <- lm(y~x.factor,contrasts=list(x.factor="contr.SAS"))
#The contrasts option for the lm() function specifies how the levels of that factor object should be used within the function. The levels option to the factor() function allows specification of the ordering of levels

#Linear models stratified by each value of a grouping variable
uniquevals <- unique(z)
numunique <- length(uniquevals)
formula <- as.formula(y~x1+...+xk)
p <- length(coef(lm(formula)))
params <- matrix(rep(0,numunique*p),p,numunique)
for (i in 1:length(uniquevals))
    {
        cat(i,"\n")
        params[,i] <- coef(lm(formula,subset=(z==uniquevals[i])))
    }

#Log-likelihood
lm1 <- lm(y~x1+x2+...+xk)
logLik(lm1)
AIC(lm1)
install.packages(nlme)
require(nlme)
BIC(lm1)


#Tests of equality of parameters
lm1 <- lm(y~x1+x2+...+xk,data=ds)
lm2 <- lm(y~I(x1+x2)+...+xk,data=ds)
anova(lm2,lm1)
##OR
rquire(gmodels)
fit.contrast(lm1,"x1",values)



#Studentized residuals
stanardized.resid.varname <- stdres(lm1)
studentized.resid.varname <- studres(lm1)

#Leverage
#Hat matrix
leverage.varname <- hatvalues(lm1)

#Cook's D
cookd.varname <- cooks.distance(lm1)

#DFFITS
#DFFITS are a stanardized function of the difference between the predicted value for the observation when it is included in the dataset and when(only) it is excluded from the dataset. They are used as a indicator of the observation's influence.

dffits.varname <- dffits(lm1)


#Plot prediction limits from a simple linear regression
pred.w.plim <- predict(lm(y~x),interval="pred")
matplot(new$x,pred.w.plim,lty=c(1,2,2),type="1",ylab="predicted y")
#The matplot() function is used to generate lines, with a solid line(lty=1), for predicted values and dashed line(lty=2) for the confidence bounds


#Covariance Matrix
varcov <- vcov(lm1)



#############################################################
#Regression Generalization
##########################################################
#Generalized linear models
glm1 <- glm(y~x1+...+xk,binomial,data=ds)
rquire(Design)
lrm1 <- lrm(y~x1+...+xk,data=ds)

#EXact logistic regressiom

#An exact test is generated for each variable listed in the exact statement, including if desired the intercept. Not all covariates in the model statement need be included in the exact statement, but all covariates in the exact statement must be included in the model statement.

install.packages("elrm")
elrmres <- elrm(y~x1+...+xk,iter=10000,burnIn=10000,data=ds)
#The elrm() function implements a modified MCMC algorithm to approximate exact conditional inference for logistic regression models


#######
#Poisson model
poissonmodel <- glm(y~x1+...+xk,poisson, data=ds)

#One way to assess the fit of the model is by comparing the observed and expected cell counts, and then calculating Pearson's chi-square statistic. This can be carried out using the goodfit() function
install.packages(vcd)
require(vcd)
poisfit <- goodfit(x,"poisson")
#The goodfit() function carried out a Pearson's chi-square test of observed vs. expected counts. Other distributions supported include binomial and nbinomial. R can also create a hanging rootogram to assess the goodness if fit for count models. If the model fits well, then the bottom of each bar in the rootogram should be near zero.

rootogram(poisfit)

###########
#Zero-inflated Poisson model

#Zero-inflated Poisson models can be used for count outcomes that generally follow a Poisson distribution but for which there are more observed counts of 0 than would be expected. These data can be seen as deriving from a mixture distribution of a Poisson and a degenerate distribution with point mass at zero.
install.packages("pscl")
require(pscl)
mod <- zeroinfl(y~x1+...+xk|x2+...+xp,data=ds)


##Negative binomial model
require(MASS)
glm.nb(y~x1+...+xk,data=ds)
#Zero-inflated negative binomial model
mod <- zeroinfl(y~x1+...+xk|x2+...+xp,data=ds,dist="negbin")


#Log-linear model
#loglinear models are a flexible approach to analysis of categorical data. A loglinear model of a three-dimensional contingency table denoted by X1,X2, and X3 might assert that the expected counts depend on a 2 way interaction between the first two variables, but that X3 is independent of all the others

logres <- loglin(table(x1,x2,x3),margin=list(c(1,2),c(3)),param=T)
pvalue <- 1-
      



