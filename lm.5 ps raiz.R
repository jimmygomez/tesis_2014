#******contruccion de un DBCA**********
getwd()
setwd("c:/BorradoR")
e=read.csv("dataenf.csv")
#analisis del peso seco. data enfermos. peso gr.
e=((e[1:192,5])*1000)^(0.1)
library(agricolae)
he<-hist(e, main= "ddi ps raiz", breaks=10, ylim=c(0,70), xlab="peso", ylab="densidad",col="green")
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
######
etiqueta<-list(c("ue1","ue2","ue3","ue4","ue5","ue6"),c("antenf","mexenf","zutenf","nabenf"),c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
myarray <- array(data =y, dim = c(6, 4, 8), dimnames = etiqueta)
library(pastecs)
stat.desc(myarray,p=0.95)
###########
amod <- aov(y~ patron+ddi+ddi*patron, data=problema)

#presentacion a b c
library(agricolae)
s1<-HSD.test(amod, "patron", group=TRUE)
s2<-waller.test(amod, "patron", group=TRUE)

#####
library(agricolae)
HSD.test(problema.aov,"patron", "ddi","ddi:patron", group=FALSE)
#####
#Mendiburu.
par(mfrow=c(1,1)) 
c1<-colors()[480]; c2=colors()[65]; c3=colors()[51]; 
c4=colors()[140] 
G1<-bar.group(s1$groups, ylim=c(1.25,1.6),ylab="pormedio^0.1", xlab="Patrones",lwd=3, main="Peso Seco Raiz Inoculada\nTukey",col=c1,las=1, borde="blue") 
G2<-bar.group(s1$groups, horiz=TRUE, xlim=c(1.25,1.6),main="peso seco raiz inoculada\nTukey",col=c2,las=1) 
G3<-bar.err(s2$means, variation="std",ylim=c(1.25,1.45),ylab="promedio^0.1", xlab="Patrones",  col=c1,main="Peso Seco Raiz Inoculada\nDesviación Estandar",las=1, lwd=3,borde="blue") 
G4<-bar.err(s2$means, variation="SE",horiz=TRUE, xlim=c(1.25,1.45),col=c4, main="Error estándar peso seco raiz inoculado\nTukey",las=1)

#mendiburuG3

par(mfrow=c(1,1))
plot(y~ddi,data=problema)
title("a) Diagrama de cajas ddi peso seco raiz")
plot(y~patron,data=problema)
title("b) Diagrama de cajas patron peso seco raiz")
plot.design(problema,fun="mean")
title("c) Medias peso seco raiz")
plot.design(problema,fun="median")
title("d) Medianas peso seco raiz")
interaction.plot(ddi,patron,y, col="RED")
title("e) Poligonos interacción factores")

plot(amod)

