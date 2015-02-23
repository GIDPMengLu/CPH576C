require("aod")
require("ggplot2")
require("foreign")
mydata<-read.dta("/Users/mlu/Courses/CPH576C/Data/frmgham.dta")
head(mydata)
summary(mydata)
require(fields)
summ1<-stats(mydata)
pdf("/Users/mlu/Courses/CPH576C/Homework/histgram_of_cvd")
hist(mydata$cvd)
table(angina~period);table(hospmi~period);table(mi_fchd~period)
table(anychd~period);table(stroke~period);table(cvd~period)
table(hyperten~period)
Uframe<- mydata
attach(Uframe)
library(plyr)
arrange(Uframe,desc(randid))
Uframe$exmtime <- 0
final_fu= max(timeap, timemi, timemifc, timechd, timestrk, timecvd, timehyp)
if (first.randid) 
     endtime=final_fu
     exmtime=time
else
    endtime=exmtime
    exmtime=time
Uframe$unique.mi <- 0
Uframe$unique.ang <- 0
Uframe$bad.heart <- 0
Uframe$inc.badheart <- 0
if(hospmi==1 | angina==1)
    inc.badheart=1
if(hospmi==1 & timemi>exmtime & timemi<=endtime)
    unique.mi=1
if(angina ==1 & timeap > exmtime & timeap <= endtime)
    unique.ang=1
if(unique.mi==1 | unique.ang==1)
    bad.heart=1
arrange(Uframe,desc(randid))
detach(Uframe)
timeck <- Uframe[which(angina==1&hospmi==1&unique.mi!=unique.ang),]

frame1 <- Uframe
attach(frame1)
if(sex=="male")
    sex=0
frame1 <- frame1[period==1,]
frame1 <- frame1[prevap!=1,]
frame1 <- frame1[prevmi!=1,]
frame1 <- frame1[prevchd!=1,]
require(Hmisc)
rcorr(as.matrix(frame1))
summ2<-stats(frame1[,c("age","sysbp","diabp","cigpday","totchol","bmi","glucose","heartrte")])
glm1 <- glm(inc.badheart~sex+age+sysbp+diabp+cigpday+totchol 
bmi+diabetes+heartrte+prevstrk+prevhyp,data=frame1,family="binomial")
summary(glm1)
detach(frame1)



#Does quitting smoking reduce cvd
#identify prevalent smoking population. identify quitters among smoking population
#sort by ascending period!
smoke <- Uframe
smoke$smoke1 <- 0
attach(smoke)
if(first.randid)
    if(cursmoke==1)
        smoker1=1
        quitter=0
     else
         smoker1=0
    if(smoker==1 % period==2)
        if(cursmoke==0)
            quitter=1
            smoker1=0
         else
             quitter=0
detach(smoke)
smoke2 <- smoke
smoke2 <- smoke2[pervap!=1,]
smoke2 <- smoke2[prevmi!=1,]
smoke2 <- smoke2[prevchd!=1,]
glm2 <- glm(inc.badheart~sex+age+sysbp+diabp+quitter+totchol 
bmi+diabetes+heartrte+prevstrk+prevhyp,data=smoke2,family="binomial")
summary(glm2)






data1 <- subset(mydata,prevap!=1 & prevmi!=1)
attach(data1)
data1$exmtime <- 0
final.fu <- max(timeap, timemi, timemifc, timechd, timestrk, timecvd, timehyp)
if(first.randid)
    endtime=final.fu
    exmtime=time
else
    endtime=exmtime
    exmtime=time
data1$unique.mi <- 0
data1$unique.ang <- 0
data1$bad.heart <- 0
if(hospmi=1 & timemi>exmtime & timemi<=endtime)
    unique.mi=1
if(angina=1 & timeap>exmtime & )

