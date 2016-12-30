#datos de peso fresco
getwd()
setwd("c:/BorradoR")
DATA<-read.csv("1ANT.csv") # gr, peso fresco
e<-(DATA[1:96,12]/1000)**0.1;e
library(agricolae)
he<-hist(e, main= "relación L/psr", breaks=8, ylim=c(0,40), xlab="Longitud/peso seco", ylab="densidad",col="green")
normal.freq(he,col="blue")
sd(e)/mean(e)*100

patron<-c(rep(1,6),rep(2,6))
ddi<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12),rep(8,12))
patron<-factor(patron,labels=c("antenf","antsan"))
ddi<-factor(ddi,labels=c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
y<-e
split(round(y,3),patron)
problema<-data.frame(y,ddi,patron)
#problema: es una dataframe
rm(y,ddi,patron)
attach(problema)
problema.aov<-aov(y~patron+ ddi+ ddi*patron,data=problema)
summary(problema.aov);medias<-model.tables(problema.aov,type="means");medias
cv.model(problema.aov) 
mean(y)
##
df<-df.residual(problema.aov)
MSerror<-deviance(problema.aov)/df 
comparap <- LSD.test(y,patron,df,MSerror)

###mexicano
getwd()
setwd("c:/BorradoR")
DATA<-read.csv("2MEX.csv") # gr, peso fresco
e<-(DATA[1:96,3]*1000)**0.1;e
library(agricolae)
he<-hist(e, main= "relación L/psr", breaks=10, ylim=c(0,40), xlab="Longitud/peso seco", ylab="densidad",col="green")
normal.freq(he,col="blue")
sd(e)/mean(e)*100

patron<-c(rep(1,6),rep(2,6))
ddi<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12),rep(8,12))
patron<-factor(patron,labels=c("mexenf","mexsan"))
ddi<-factor(ddi,labels=c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
y<-e
split(round(y,3),patron)
problema<-data.frame(y,ddi,patron)
#problema: es una dataframe
rm(y,ddi,patron)
attach(problema)
problema.aov<-aov(y~patron+ ddi+ ddi*patron,data=problema)
summary(problema.aov);medias<-model.tables(problema.aov,type="means");medias
cv.model(problema.aov) 
mean(y)
##
df<-df.residual(problema.aov)
MSerror<-deviance(problema.aov)/df 
comparap <- LSD.test(y,patron,df,MSerror)

#ZUTANO
getwd()
setwd("c:/BorradoR")
DATA<-read.csv("3ZUT.csv") # gr, peso fresco
e<-(DATA[1:96,3]*1000)**0.1;e
library(agricolae)
he<-hist(e, main= "relación L/psr", breaks=10, ylim=c(0,40), xlab="Longitud/peso seco", ylab="densidad",col="green")
normal.freq(he,col="blue")
sd(e)/mean(e)*100

patron<-c(rep(1,6),rep(2,6))
ddi<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12),rep(8,12))
patron<-factor(patron,labels=c("zutenf","zutsan"))
ddi<-factor(ddi,labels=c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
y<-e
split(round(y,3),patron)
problema<-data.frame(y,ddi,patron)
#problema: es una dataframe
rm(y,ddi,patron)
attach(problema)
problema.aov<-aov(y~patron+ ddi+ ddi*patron,data=problema)
summary(problema.aov);medias<-model.tables(problema.aov,type="means");medias
cv.model(problema.aov) 
mean(y)
##
df<-df.residual(problema.aov)
MSerror<-deviance(problema.aov)/df 
comparap <- LSD.test(y,patron,df,MSerror)

#NABAL
getwd()
setwd("c:/BorradoR")
DATA<-read.csv("4NAB.csv") # gr, peso fresco
e<-(DATA[1:96,3]*1000)**0.1;e
library(agricolae)
he<-hist(e, main= "relación L/psr", breaks=10, ylim=c(0,40), xlab="Longitud/peso seco", ylab="densidad",col="green")
normal.freq(he,col="blue")
sd(e)/mean(e)*100

patron<-c(rep(1,6),rep(2,6))
ddi<-c(rep(1,12),rep(2,12),rep(3,12),rep(4,12),rep(5,12),rep(6,12),rep(7,12),rep(8,12))
patron<-factor(patron,labels=c("nabenf","nabsan"))
ddi<-factor(ddi,labels=c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
y<-e
split(round(y,3),patron)
problema<-data.frame(y,ddi,patron)
#problema: es una dataframe
rm(y,ddi,patron)
attach(problema)
problema.aov<-aov(y~patron+ ddi+ ddi*patron,data=problema)
summary(problema.aov);medias<-model.tables(problema.aov,type="means");medias
cv.model(problema.aov) 
mean(y)
##
df<-df.residual(problema.aov)
MSerror<-deviance(problema.aov)/df 
comparap <- LSD.test(y,patron,df,MSerror)
