## AktStat
## Woche 7
## Dr. Tino Werner
#############################################################

require(CASdatasets)
data(usmassBI2)


str(usmassBI2)
head(usmassBI2)


## use data from 1993-1997 for training and from 1998 for validation

AutoClaimTrain<-subset(usmassBI2,YEAR<1998)

## Wir versuchen hier, Städte-spezifische Effekte aus dem Plot abzulesen

plot(AC~YEAR,data=AutoClaimTrain,ylab='Average claim',xlab='Year')
for(i in AutoClaimTrain$TOWNCODE){
  lines(AC~YEAR,data=subset(AutoClaimTrain,TOWNCODE==i))
}



## Scatterplot, um Relationen zwischen AC, PPSM und PCI zu entdecken:

AutoClaimTrain$lnPCI<-log(AutoClaimTrain$PCI)
AutoClaimTrain$lnPPSM<-log(AutoClaimTrain$PPSM)

plot(AC~lnPCI,data=AutoClaimTrain,ylab='Average claim',xlab='PCI')
for(i in AutoClaimTrain$TOWNCODE){
  lines(AC~lnPCI,data=subset(AutoClaimTrain,TOWNCODE==i))
}

plot(AC~lnPPSM,data=AutoClaimTrain,ylab='Average claim',xlab='PPSM')
for(i in AutoClaimTrain$TOWNCODE){
  lines(AC~lnPPSM,data=subset(AutoClaimTrain,TOWNCODE==i))
}

plot(AC~YEAR,data=AutoClaimTrain,ylab='Average claim',xlab='YEAR')
for(i in AutoClaimTrain$TOWNCODE){
  lines(AC~YEAR,data=subset(AutoClaimTrain,TOWNCODE==i))
}

## Zunächst: Ohne die zeitliche Komponente zu beachten, werden alle Trainingsdaten
## gepoolt und darauf eine lineare Regression ausgeführt:


Pool.fit0<-lm(AC~lnPCI+lnPPSM,data=AutoClaimTrain)
summary(Pool.fit0)


## Die Variable YEAR ermöglicht zwar zumindest indirekt die Berücksichtigung
## der zeitlichen Verlaufs, praktisch wird allerdings in folgendem Fit lediglich 
## YEAR als eine kategorielle Variable betrachtet, d.h., sollte es spezifische
## Effekte für YEAR geben, so würden diese somit für alle Y_{it}, i=1,...,n, 
## gelten, also nicht die zeitliche Abfolge der einzelnen Beobachtungen be-
## rücksichtigen.
## Wir wollen hohe numerische Ausprägungen von YEAR vermeiden:
AutoClaimTrain$YEAR<-AutoClaimTrain$YEAR-1992
Pool.fit<-lm(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain)
summary(Pool.fit)





## Fixe Effekte (=gepoolte lineare Regression)

FE.fit0<-lm(AC~factor(TOWNCODE)+lnPCI+lnPPSM-1,data=AutoClaimTrain)
summary(FE.fit0)

FE.fit<-lm(AC~factor(TOWNCODE)+lnPCI+lnPPSM+YEAR-1,data=AutoClaimTrain)
summary(FE.fit)




## Vorverarbeitung der Testdaten:

AutoClaimTest<-subset(usmassBI2,YEAR==1998)

AutoClaimTest$lnPCI<-log(AutoClaimTest$PCI)
AutoClaimTest$lnPPSM<-log(AutoClaimTest$PPSM)
AutoClaimTest$YEAR<-AutoClaimTest$YEAR-1992


## Prädiktion

predpool0<-predict(Pool.fit0,AutoClaimTest)
predpool<-predict(Pool.fit,AutoClaimTest)
predFE0<-predict(FE.fit0,AutoClaimTest)
predFE<-predict(FE.fit,AutoClaimTest)

## alternativ ohne predict:
Xmat<-cbind(rep(1,nrow(AutoClaimTest)),AutoClaimTest$lnPCI,AutoClaimTest$lnPPSM,AutoClaimTest$YEAR)
beta.Pool<-coef(Pool.fit)
pred.Pool<-Xmat%*%beta.Pool


mean((predpool0-AutoClaimTest$AC)^2)
mean((predpool-AutoClaimTest$AC)^2)
mean((predFE0-AutoClaimTest$AC)^2)
mean((predFE-AutoClaimTest$AC)^2)




## Generalisierte lineare Regression mit Autokorrelation

## Die Korrelationsannahme betrifft die Residuen (\epsilon_{it}), i=1,...,T_i. Daher
## bestimmen wir die Korrelation der Residuen aus den beiden gepoolten Fits:
AutoClaimTrain$rPool<-resid(Pool.fit)
rvec<-cbind(subset(AutoClaimTrain,YEAR==1)$rPool,subset(AutoClaimTrain,YEAR==2)$rPool,subset(AutoClaimTrain,YEAR==3)$rPool,subset(AutoClaimTrain,YEAR==4)$rPool,subset(AutoClaimTrain,YEAR==5)$rPool)
cor(rvec)

## Hier ist eine deutlich Autokorrelation zwischen den Residuen der verschiedenen Jahre zu erkennen. Dies lässt sich auch testen, z.B.

cor.test(rvec[,1],rvec[,2])



AutoClaimTrain$rPool0<-resid(Pool.fit0)
rvec<-cbind(subset(AutoClaimTrain,YEAR==1)$rPool0,subset(AutoClaimTrain,YEAR==2)$rPool0,subset(AutoClaimTrain,YEAR==3)$rPool0,subset(AutoClaimTrain,YEAR==4)$rPool0,subset(AutoClaimTrain,YEAR==5)$rPool0)
cor(rvec)

## Hier ist eine deutlich Autokorrelation zwischen den Residuen der verschiedenen Jahre zu erkennen. Dies lässt sich auch testen, z.B.

cor.test(rvec[,1],rvec[,2])

## R(tau) wird durch getVarCov geschätzt. Hier werden jeweils unterschiedliche Strukturen von Autokorrelation angenommen, nämlich Compound-Symmetrie, AR(1) und unstrukturiert. Compound-Symmetrie bedeutet, dass außerhalb der Diagonalen eine Konstante rho steht, bei AR(1) steht in der k-ten Nebendiagonale rho^k, und die unstrukturierte Autokorrelation erlaubt beliebige Einträge \sigma_{ij} in R.

require(nlme)



## Compound-Symmetrie
SCex.fit0<-gls(AC~lnPCI+lnPPSM,data=AutoClaimTrain,correlation=corCompSymm(form=~1|TOWNCODE))
summary(SCex.fit0)
intervals(SCex.fit0,which="var-cov")
getVarCov(SCex.fit0)

## AR(1)
SCar.fit0<-gls(AC~lnPCI+lnPPSM,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE))
summary(SCar.fit0)
intervals(SCar.fit0,which="var-cov")
getVarCov(SCar.fit0)

## Symmetrie
SCun.fit0<-gls(AC~lnPCI+lnPPSM,data=AutoClaimTrain,correlation=corSymm(form=~1|TOWNCODE))
summary(SCun.fit0)
intervals(SCun.fit0,which="var-cov")
getVarCov(SCun.fit0)


## Compound-Symmetrie
SCex.fit<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corCompSymm(form=~1|TOWNCODE))
summary(SCex.fit)
intervals(SCex.fit,which="var-cov")
getVarCov(SCex.fit)

## AR(1)
SCar.fit<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE))
summary(SCar.fit)
intervals(SCar.fit,which="var-cov")
getVarCov(SCar.fit)

## Symmetrie
SCun.fit<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corSymm(form=~1|TOWNCODE))
summary(SCun.fit)
intervals(SCun.fit,which="var-cov")
getVarCov(SCun.fit)



SCex.fit.ml<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corCompSymm(form=~1|TOWNCODE),method='ML')
SCar.fit.ml<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE),method='ML')

SCun.fit.ml<-gls(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,correlation=corSymm(form=~1|TOWNCODE),method='ML')

## Prädiktion

beta.SCar<-coef(SCar.fit)
pred.SCar<-Xmat%*%beta.SCar

## mit predict
head(predict(SCar.fit,AutoClaimTest))


mean((predict(SCex.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(SCex.fit,AutoClaimTest)-AutoClaimTest$AC)^2)

mean((predict(SCar.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(SCar.fit,AutoClaimTest)-AutoClaimTest$AC)^2)

mean((predict(SCun.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(SCun.fit,AutoClaimTest)-AutoClaimTest$AC)^2)



## Fixe Effekte und Autokorrelation:

FEar.fit<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM+YEAR-1,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE))
FEar.fit0<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM-1,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE))

## vgl. MLE:
FEar.fit.ml<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM+YEAR-1,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE),method='ML')
FEar.fit.ml0<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM-1,data=AutoClaimTrain,correlation=corAR1(form=~1|TOWNCODE),method='ML')


FEex.fit<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM+YEAR-1,data=AutoClaimTrain,correlation=corCompSymm(form=~1|TOWNCODE))
FEex.fit0<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM-1,data=AutoClaimTrain,correlation=corCompSymm(form=~1|TOWNCODE))


FEun.fit<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM+YEAR-1,data=AutoClaimTrain,correlation=corSymm(form=~1|TOWNCODE))
FEun.fit0<-gls(AC~factor(TOWNCODE)+lnPCI+lnPPSM-1,data=AutoClaimTrain,correlation=corSymm(form=~1|TOWNCODE))


mean((predict(FEex.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(FEex.fit,AutoClaimTest)-AutoClaimTest$AC)^2)

mean((predict(FEar.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(FEar.fit,AutoClaimTest)-AutoClaimTest$AC)^2)

mean((predict(FEun.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(FEun.fit,AutoClaimTest)-AutoClaimTest$AC)^2)


#######################################################
## Zufällige Effekte
#######################################################

RE.fit<-lme(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,random=~1|TOWNCODE)
summary(RE.fit)

RE.fit0<-lme(AC~lnPCI+lnPPSM,data=AutoClaimTrain,random=~1|TOWNCODE)
summary(RE.fit0)

## Alternativ: Funktion lmer aus dem Paket lme4

require(lme4)


RE.fitr<-lmer(AC~(1|TOWNCODE)+lnPCI+lnPPSM+YEAR,data=AutoClaimTrain)
summary(RE.fitr)




## EC.fit durch AR(1)-Korrelationsstruktur aufdatieren:

REar.fit<-update(RE.fit,correlation=corAR1(form=~1|TOWNCODE))
summary(REar.fit)

intervals(REar.fit,which="var-cov")

getVarCov(REar.fit)

getVarCov(REar.fit,type='conditional')

getVarCov(REar.fit,type='marginal')

REar.fit0<-update(RE.fit0,correlation=corAR1(form=~1|TOWNCODE))
summary(REar.fit0)


EC.fit.ml<-lme(AC~lnPCI+lnPPSM+YEAR,data=AutoClaimTrain,random=~1|TOWNCODE,method='ML')
RE.fit.ml<-update(EC.fit.ml,correlation=corAR1(form=~1|TOWNCODE),method='ML')


REex.fit<-update(RE.fit,correlation=corCompSymm(form=~1|TOWNCODE))
summary(REex.fit)
REex.fit0<-update(RE.fit0,correlation=corCompSymm(form=~1|TOWNCODE))
summary(REex.fit0)

REun.fit<-update(RE.fit,correlation=corSymm(form=~1|TOWNCODE))
summary(REun.fit)
REun.fit0<-update(RE.fit0,correlation=corSymm(form=~1|TOWNCODE))
summary(REun.fit0)



## auch möglich: Modellierung der zufälligen Effekte mit der Variable YEAR
REslope.fit0<-lme(AC~lnPCI+lnPPSM,data=AutoClaimTrain,random=(~1+YEAR|TOWNCODE))
summary(REslope.fit0)

## Prädiktion



alpha.RE<-ranef(RE.fit)
beta.RE<-fixef(RE.fit)
pred.RE<-alpha.RE+Xmat%*%beta.RE
## oder einfach mit predict
pred2<-predict(RE.fit,AutoClaimTest)
head(pred.RE)
head(pred2)


mean((predict(RE.fit,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(RE.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REslope.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REar.fit,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REar.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REex.fit,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REex.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REun.fit,AutoClaimTest)-AutoClaimTest$AC)^2)
mean((predict(REun.fit0,AutoClaimTest)-AutoClaimTest$AC)^2)


####################################################
## GLMMs
####################################################

## Wir verwenden hier nun die Variable PPSM als Response-Variable:

boxplot(AutoClaimTrain$AC,AutoClaimTrain$PPSM)

## Dort lässt sich am ehesten eine Rechtsschiefe erkennen, die eine Normal-
## verteilungsannahme fragwürdig machen könnte.


require(lme4)

AutoClaimTrain$PPSM1000<-AutoClaimTrain$PPSM/1000
AutoClaimTrain$lnAC<-log(AutoClaimTrain$AC)

glmmres <- glmer(PPSM1000 ~ (1|TOWNCODE),data=AutoClaimTrain, 
                 family=Gamma(link='log'))



summary(glmmres)


## get fixed effects
fixef(glmmres)

## get random intercepts
int <- ranef(glmmres)$TOWNCODE



glmmres2 <- glmer(PPSM1000 ~ (1|TOWNCODE)+lnAC,data=AutoClaimTrain, 
                  family=Gamma(link='log'))


summary(glmmres2)


## get fixed effects
fixef(glmmres2)

## get random intercepts
int <- ranef(glmmres2)$TOWNCODE