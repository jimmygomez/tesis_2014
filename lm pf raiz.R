#******contruccion de un DBCA**********
getwd()
setwd("c:/BorradoR")
e=read.csv("xlsENFE.csv")
e=e[1:192,4]
patron<-c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
ddi<-c(rep(1,24),rep(2,24),rep(3,24),rep(4,24),rep(5,24),rep(6,24),rep(7,24),rep(8,24))
patron<-factor(patron,labels=c("antenf","mexenf","zutenf","nabenf"))
ddi<-factor(ddi,labels=c("15ddi","30ddi","44ddi","60ddi","76ddi","109ddi","141ddi","172ddi"))
y<-log10(x=e^-1)
split(round(y,3),patron)

problema<-data.frame(y,ddi,patron)
rm(y,ddi,patron)
attach(problema)
problema.aov<-aov(y~ddi*patron,data=problema)
problema.aov
summary(problema.aov)
medias<-model.tables(problema.aov,type="means");medias


par(mfrow=c(1,1))
plot(y~ddi,data=problema)
title("a) Diagrama de cajas")
plot(y~patron,data=problema)
title("b) Diagrama de cajas")
plot.design(problema,fun="mean")
title("c) Medias")
plot.design(problema,fun="median")
title("d) Medianas")
interaction.plot(patron,ddi,y)
title("e) Poligonos")


simple.aov<-aov(y~ddi,data=problema)
summary(simple.aov)
Df Sum of Sq Mean Sq F Value Pr(F)

ECM<-deviance(simple.aov)/simple.aov$df.residual;ECM

resstd<-residuals(simple.aov)/sqrt(ECM)
resstd
par(mfrow=c(2,2))
boxplot(resstd,xlab="residuos estandarizados")
title("a) Diagrama de caja")
qqnorm(resstd,xlab="cuantiles de la normal",ylab="residuos est.")
qqline(resstd)
title("b) QQ-plot")
plot((1:length(resstd)),resstd,type="p",xlab="i",ylab="residuos est.")
title("c) residuos est. vs index")
plot(fitted(simple.aov),resstd,xlab="predicciones",ylab="residuos est.")

library(agricolae)
hy<-hist(y, main= "ddi pf raiz", xlab="peso", ylab="densidad",col="green")
normal.freq(hy,col="blue")

he<-hist(e, main= "ddi pf raiz", xlab="peso", ylab="densidad",col="green")
normal.freq(he,col="blue")


e^(1)
sd(e)/mean(e)*100

y1<-log2(x=y)
sd(y1)/mean(y1)*100
ks.test(y1,mean(y1),sd(y1))
