## Aktuarielle Statistik
## Woche 5
## Dr. Tino Werner
##############################################

############################################################################
## Individuelle Modellierung der Schadenanzahl
############################################################################



require(CASdatasets)

data(freMTPLfreq)
data(freMTPLsev)
data(freMTPL2freq)
data(freMTPL2sev)




Df<-freMTPLfreq
Df$DriverAge<-cut(Df$DriverAge,c(17,22,26,42,74,Inf))
Df$CarAge<-cut(Df$CarAge,c(0,1,4,15,Inf),include.lowest=T)
Df$Density<-cut(Df$Density,c(0,40,200,500,4500,Inf),include.lowest=T)


## Poisson-GLM

res<-glm(ClaimNb~DriverAge+Density+offset(log(Exposure)),family=poisson,data=Df)
summary(res)

## Exposure entspricht für KfZ-Policen hier der Anzahl der versicherten Jahre,
## d.h., für eine Poisson-Verteilung, welche die Schadenanzahl pro Jahr modelliert,
## muss die Exposure mit der jährlichen Claimrate multipliziert werden

## log(Exposure) wird als offset im Poisson-GLM verwendet, da der Offset als
## Achsenabschnitt verstanden werden kann, welcher allerdings auf der Link-Skala
## lebt, d.h., ln(Y_i)=ln(e_i)+X_i\beta <=> offset_i=ln(e_i)

str(res)

##################################################################################
## Erinnerung: Beim gewöhnlichen linearen Modell sind die Koeffizienten für die
## kategoriellen Variablen nichts weiter als die Mittelwerte der Response-aus-
## prägungen für alle Instanzen der entsprechenden Kategorie.
lm(Sepal.Length~Species-1,iris)
tapply(iris$Sepal.Length,iris$Species,mean)
## Für GLMs muss man beachten, dass dies für die Score auf der Response-Skala
## auch richtig ist, nur wird für die Koeffizienten noch die Link-Funktion da-
## zwischen geschaltet:
glm(Gas~CarAge-1,family=binomial(link="logit"),data=Df)
(Gastab<-aggregate(Df$CarAge,list(Df$Gas),summary))
(relfreqs<-Gastab[2,]$x/(Gastab[1,]$x+Gastab[2,]$x))
log(relfreqs/(1-relfreqs))
####################################################################################


## Zurück zum Poisson-GLM:
## Nur Gas als erklärende Variable:

tapply(Df$ClaimNb,Df$Gas,sum)

## bzw. exposuregewichtet

tapply(Df$ClaimNb,Df$Gas,sum)/tapply(Df$Exposure,Df$Gas,sum)


respois<-glm(ClaimNb~0+Gas+offset(log(Exposure)),family=poisson(link='log'),data=Df)

exp(coefficients(respois))

predict(respois,newdata=data.frame("Exposure"=c(1,1),"Gas"=c("Diesel","Regular")),type='response')


## Beim Integrieren eines Achsenabschnitts muss man auf die Bedeutung der 
## Koeffizienten achten:

respois2<-glm(ClaimNb~Gas+offset(log(Exposure)),family=poisson(link='log'),data=Df)

respois2

exp(coefficients(respois2))

prod(exp(coefficients(respois2)))

predict(respois2,newdata=data.frame("Exposure"=c(1,1),"Gas"=c("Diesel","Regular")),type='response')





## Regression mit der in Klassen zerhackten Variable age

reg.cut<-glm(ClaimNb~DriverAge+offset(log(Exposure)),family=poisson,data=Df)
summary(reg.cut)



## Modell mit allen Variablen außer Region, Density und PolicyID

respois3<-glm(ClaimNb~Power+CarAge+DriverAge+Brand+Gas+offset(log(Exposure)),family=poisson(link='log'),data=Df)


## Volles Modell 


respoisfull<-glm(ClaimNb~Power+CarAge+DriverAge+Brand+Gas+Region+Density+offset(log(Exposure)),family=poisson(link='log'),data=Df)




## Zurück zum initialen Modell

## Test auf Überdispersion:

require(AER)
dispersiontest(res)

## Poisson-Modell war nicht geeignet, also hat zu wenig Varianz abgebildet.



## NB-Modelle


require(MASS)
res.nb<-glm(ClaimNb~DriverAge+Density+offset(log(Exposure)),family=negative.binomial(1),data=Df)
summary(res.nb)

## Wenn phi nicht bekannt ist, mit glm.nb gemeinsam mit beta schätzen:

res.nb2<-glm.nb(ClaimNb~DriverAge+Density+offset(log(Exposure)),data=Df)
summary(res.nb2)





## Zero-inflated Poisson model


require(pscl)

regzip<-zeroinfl(ClaimNb~DriverAge+CarAge+Density+Brand+Power+Gas+offset(log(Exposure))|1,data=Df,dist='poisson',link='logit')
summary(regzip)

## Hier ist \pi_i nur eine Konstante. Man kann allerdings auch \pi_i selbst durch
## Variablen modellieren:

regzip2<-zeroinfl(ClaimNb~DriverAge+CarAge+Density+Brand+Power+Gas+offset(log(Exposure))|DriverAge,data=Df,dist='poisson',link='logit')
summary(regzip2) 







########################################################
## Bisher: Auswahl der Variablen per Hand
## Nun: Automatische Modellwahl
########################################################


##  Lasso:

require(glmnet)

Dmod<-makeX(Df[,-c(1,2,3)])

reslassoraw<-glmnet(x=Dmod,y=Df$ClaimNb,family=poisson,offset=log(Df$Exposure))

plot(reslassoraw,xvar='lambda',label=T)

reslasso<-cv.glmnet(x=Dmod,y=Df$ClaimNb,family=poisson,nfolds=5,offset=log(Df$Exposure))

reslasso

plot(reslasso)

reslassomin<-glmnet(x=Dmod,y=Df$ClaimNb,family=poisson,lambda=reslasso$lambda.min,offset=log(Df$Exposure))

reslasso1se<-glmnet(x=Dmod,y=Df$ClaimNb,family=poisson,lambda=reslasso$lambda.1se,offset=log(Df$Exposure))

coef(reslassomin)
coef(reslasso1se)



## Für Elastic Net würde man zusätzlich noch das Argument alpha eingeben, welches
## per Default 1 ist, und auf einen Wert in ]0,1[ setzen. alpha=0 ist die Ridge-
## Regression.



## Boosting:

require(mboost)

## Hier ist die Bedienung wieder genau wie bei glm:

boostpois<-glmboost(ClaimNb~.-PolicyID,data=Df,family=Poisson())

## Exposure lässt sich allerdings nicht so integrieren wie bei glmnet oder glm.



## Stabilitätsselektion
## Hier aufgrund der Laufzeit nur auf einem reduzierten Datensatz:

Dfred<-Df[1:10000,]

## Die Stabilitätsselektion liegt in den Paketen stabs und mboost als stabsel vor
## Für Lasso-Modelle ist die Bedienung ähnlich wie glmnet, jedoch muss bei der
## Verwendung eines family-Arguments dieses durch args.fitfun übergeben werden.
## Zudem sind von den drei Parameterm
## q: Mittlere Anzahl an Variablen pro Modell
## PFER: per-family error rate (erwartete Anzahl an fälschlicherweise selektier-
##       ten, aber nicht relevanten Variablen)
## cutoff: Threshold, sodass alle Variablen, deren relative Selektionshäufigkeit
##       über alle Modelle mindestens so groß ist wie der Cutoff, ins stabile
##       Modell einziehen
## zwei zu spezifizieren, was in der Praxis keineswegs trivial ist.

stablasso<-stabsel(Dmod[1:10000,],Dfred$ClaimNb,fitfun=glmnet.lasso,args.fitfun=list(family=poisson),PFER=1,cutoff=0.9)



############################################################################
## Individuelle Modellierung der Schadenhöhe
############################################################################



## Das Paket CASdatasets enthält den Datensatz freMTPLsev, welcher zu unserem
## Datensatz Df korrespondiert und für die Policy-IDs, welche zu einem Schaden
## führten, die Schadenhöhe enthalten.
## Man beachte, dass all jene Policy-IDs ohne Schaden nicht aufgeführt werden,
## daher hat der Datensatz weniger Instanzen als Df. 
## Beide Datensätze lassen sich mit dem Befehl merge verbinden:
Dfcl<-merge(Df,freMTPLsev)
dim(Dfcl)

## Es ist zu sehen, dass alle Instanzen ohne Schaden fallen gelassen worden sind.
## Das ist auch richtig, denn die folgenden Modelle setzen eine Response >0
## voraus

## Lognormal-Modell
reg.logn<-lm(log(ClaimAmount)~CarAge+Gas,data=Dfcl[Dfcl$ClaimAmount<15000,])
summary(reg.logn)

reg.gamma<-glm(ClaimAmount~CarAge+Gas,family=Gamma(link='log'),data=Dfcl[Dfcl$ClaimAmount<15000,])
summary(reg.gamma)


## Gamma-Boosting

resgammaboost<-glmboost(ClaimAmount~.-PolicyID,family=GammaReg(),data=Dfcl[Dfcl$ClaimAmount<15000,])


## Aufteilung in kleine und große Claims für die separate Modellierung

## Hier einmal ohne die Diskretisierung der Variablen
Df<-freMTPLfreq
Dfcl<-merge(Df,freMTPLsev)




s<-10000

mean(Dfcl$ClaimAmount<s)
## D.h., etwa 1.7% der Claims werden hier nicht erfasst

Dfcl$Standard<-1*(Dfcl$ClaimAmount<s)
## Hier wird nun ein logistisches polynomiales Modell gefittet, um die Wkeit, einen
## kleinen Claim zu verursachen, bedingt auf das Alter des Fahrers, zu modellieren:


age<-seq(18,100)
regLargeClaim<-glm(Standard~poly(DriverAge,3),data=Dfcl,family=binomial)
YpC<-predict(regLargeClaim,newdata=data.frame(DriverAge=age),type='response',se=T)

plot(age,YLargeClaim$fit,ylim=c(0.95,1),type='l')
polygon(c(age,rev(age)),c(YLargeClaim$fit+2*YLargeClaim$se.fit,rev(YLargeClaim$fit-2*YLargeClaim$se.fit)),col='grey',border=NA)
abline(h=mean(Dfcl$Standard),lty=2)
points(age,YLargeClaim$fit,type='l')

## Indices der Instanzen mit normalen Claims

indexst<-which(Dfcl$ClaimAmount<s)

## Gamma-GLM für kleine Claims

regA<-glm(ClaimAmount~poly(DriverAge,3),data=Dfcl[indexst,],family=Gamma(link='log'))
ypA<-predict(regA,newdata=data.frame(DriverAge=age),type='response')
summary(regA)


## Gamma-GLM für große Claims

regB<-glm(ClaimAmount~poly(DriverAge,3),data=Dfcl[-indexst,],family=Gamma(link='log'))
ypB<-predict(regB,newdata=data.frame(DriverAge=age),type='response')
summary(regB)

## Gamma-GLM für alle Claims

reg<-glm(ClaimAmount~poly(DriverAge,3),data=Dfcl,family=Gamma(link='log'))
yp<-predict(reg,newdata=data.frame(DriverAge=age),type='response')


plot(age,yp,type='l',lwd=2,ylab='Average cost',xlab='Age of the driver')
lines(age,YpC$fit*ypA+(1-YpC$fit)*ypB,type='h',col='lightgrey',lwd=6)
lines(age,YpC$fit*ypA,type='h',col='grey',lwd=6)
lines(age,yp,type='l',lwd=2)
abline(h=mean(Dfcl$ClaimAmount),lty=2)


############################################################################
## Individuelle Modellierung des Gesamtschadens
############################################################################


## Berechnung des Gesamtschadens pro Police
A<-tapply(freMTPLsev$ClaimAmount,freMTPLsev$PolicyID,sum)
ADF<-data.frame(PolicyID=as.numeric(names(A)),ClaimAmount=as.vector(A))
Df<-freMTPLfreq
CT<-merge(Df,ADF)


require(tweedie)

## Dauert sehr lange!
out<-tweedie.profile(ClaimAmount~Power+CarAge+DriverAge+Brand+Gas+Density+offset(log(CT$Exposure)),data=CT,p.vec=seq(1.05,1.95,by=0.1),do.smooth=F)

out$p.max
plot(out,type='b')
abline(v=out$p.max,lty=2)

## Zunächst wird ein Poisson-Modell (Tweedie-Modell mit p=1) gefittet, um die
## berechneten Koeffizienten als Warm Start für die aufwändigere Berechnung der
## Tweedie-Koeffizienten zu verwenden

## Achtung: statmod laden, sonst erkennt R tweedie nicht!

require(statmod)
tweediereg<-glm(ClaimAmount~Power+CarAge+DriverAge+Brand+Gas+Density+offset(log(CT$Exposure)),data=CT,family=tweedie(var.power=1.5,link.power=0))


require(MASS)
res.nb2<-glm.nb(ClaimNb~DriverAge+Density+offset(log(Exposure)),data=Dfcl)
summary(res.nb2)





