#******contruccion de un DBCA**********
getwd()
setwd("c:/BorradoR")
e=read.csv("dataenf.csv")
#analisis del LONGITUD DE RAIZ data enfermos. (km)
e=e[1:192,10]^(0.1)
library(agricolae)
he<-hist(e, main= "ddi Long raiz", breaks=10, ylim=c(0,80), xlab="peso", ylab="densidad",col="green")
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
#########
etiqueta<-list(c("ue1","ue2","ue3","ue4","ue5","ue6"),c("antenf","mexenf","zutenf","nabenf"),c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
myarray <- array(data =y, dim = c(6, 4, 8), dimnames = etiqueta)
library(pastecs)
stat.desc(myarray,p=0.95)
#######.....

amod <- aov(y~ ddi+patron+ddi*patron, data=problema)

#presentacion a b c
library(agricolae)
s1<-HSD.test(amod, "patron", group=TRUE)
s2<-waller.test(amod, "patron", group=TRUE)

library(agricolae)
HSD.test(problema.aov,"patron", "ddi","ddi:patron", group=FALSE)

#####
#Mendiburu.
par(mfrow=c(1,1)) 
c1<-colors()[480]; c2=colors()[65]; c3=colors()[15]; 
c4=colors()[140] 
G1<-bar.group(s1$groups, ylim=c(0.95,1.15),main="Longitud Raiz Inoculada\nTukey", ylab="promedio^0.1", xlab="Patrones", col=c4,las=1, borde="blue", lwd=3) 
G2<-bar.group(s1$groups, horiz=TRUE, xlim=c(0.95,1.12),main="Longitud raiz inoculada\nTukey",col=c2,las=1) 
G3<-bar.err(s2$means, variation="std",ylim=c(0.79,1.12), col=c4,main="Longitud Raiz Inoculada\nDesviaci�n est�ndar", ylab="promedio^0.1", xlab="Patrones",lwd=3, las=1, borde="blue") 
G4<-bar.err(s2$means, variation="SE", horiz=TRUE, xlim=c(0.93,1.1),col=c4, main="longitud raiz inoculada\nError est�ndar",las=1)
####.....
#Mendiburu. fin

par(mfrow=c(1,1))
plot(y~ddi,data=problema)
title("a) Diagrama de cajas LONG  raiz ddi")
plot(y~patron,data=problema)
title("b) Diagrama de cajas Long raiz patron")
plot.design(problema,fun="mean")
title("c) Medias")
plot.design(problema,fun="median")
title("d) Medianas")
interaction.plot(ddi,patron,y, col="purple")
title("e) Poligonos interacci�n Long raiz factores")

plot(amod)

plot(amo)
