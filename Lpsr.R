#******contruccion de un DBCA**********
getwd()
setwd("c:/BorradoR")
e=read.csv("dataenf.csv")
#analisis del Long (Km)/pesoseco(gr.)
e=(e[1:192,12]/1000)^(0.1)
library(agricolae)
he<-hist(e, main= "relación L/psr", breaks=8, ylim=c(0,90), xlab="Longitud/peso seco", ylab="densidad",col="green")
normal.freq(he,col="blue")
sd(e)/mean(e)*100

patron<-c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
ddi<-c(rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24))
patron<-factor(patron,labels=c("antenf","mexenf","zutenf","nabenf"))
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
#etiqueta<-list(c("ue1","ue2","ue3","ue4","ue5","ue6"),c("antenf","mexenf","zutenf","nabenf"),c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
#myarray <- array(data =y, dim = c(6, 4, 8), dimnames = etiqueta)
#library(pastecs)
#stat.desc(myarray,p=0.95)

amod <- aov(y~ ddi+patron+ddi*patron, data=problema)

#presentacion a b c
library(agricolae)
s1<-HSD.test(amod, "patron", group=TRUE)
s2<-waller.test(amod, "patron", group=TRUE)

library(agricolae)
HSD.test(problema.aov,"patron", "ddi","ddi:patron", group=FALSE)
