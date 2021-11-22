setwd("C:/Users/Gabriele/Documents/GitHub/StatComp2021")
load("StatComp2021.RData")

# Vogliamo capire cosa spiega la frequenza delle bevute, drink days
dati<-read.csv("https://raw.githubusercontent.com/F041/StatComp2021/main/Merged_Unique_Names_V2.csv")
head(data)

### Librerie ----
library(forecast)
library(MASS)
library(car)
library(gvlma)
library(ggplot2)
library(tidyverse)
library(mctest)
library("skimr")
library(gvlma)
library(lmtest)
library(sandwich)
library(car)
options(scipen=999)

### Descriptives ----
summary(dati) # ordine differente, vedo gli NAs con più fatica
descr<-skim(dati) #non userò le covariate con più missing >2000
descr
# non user num colonna: 5,6,10:14, 16, 18:24 ....

hist(dati$age)
plot(ecdf(dati$age)) #mi serve per capire se ha senso mettere delle età nel modello



# posso automatizzare?
library(dplyr)
selezione<-drop_na(dati) #significa che tutte le righe hanno almeno un missing...

hist(dati$drinks_day) #esponenziale negativissima
plot(ecdf(dati$drinks_day))#Funzione di ripartizione. Un outilier a 80, poi 60
cor(dati[,c("income","age","drinks_day")]) #NAs give issues
pairs((dati[,c("income","age","drinks_day","bmi")]),panel = panel.smooth)

# Cosa succede se metto dentro cose con tanti NA?
pairs((dati[,c("military_insur","drinks_day")]),panel = panel.smooth)
cor(dati[,c("military_insur","drinks_day")])

cor(dati[,4:30])

cor(dati[,c("gen_health","iron")]) #controllo per gen_health, iron 
# troppi NA?

### Modifico variabili ----
dati$gender<-dati$gender%>%as.factor
dati$education<-dati$education%>%as.factor
dati$marital<-dati$marital%>%as.factor
dati$gen_health<-dati$gen_health%>%as.factor

### First model ----
# dalle descrittive osserviamo che Y non si distribuisce come una normale
# trasformeremo dopo
formula <- paste(colnames(dati), collapse="','")
lm1<-lm(drinks_day~diabetes+as.factor(depression)
        +age+gender+race+grip_strength  
        +education+bmi+marital+income+as.factor(household_size)
        +insurance
        #+private_insur+medicare+medicaid
        #+smoker
        +gen_health+iron, data=dati) 
summary(lm1)

par(mfrow=c(2,2)) 
plot(lm1)
par(mfrow=c(1,1)) 

bptest(lm1) #presenta eteros. serve sistemare l'inferenza.
coeftest(lm1, vcov=vcovHC(lm1)) 


p<-predict(lm1,dati) #serve dopo

### Controllo collinearità con TOL sotto 0,3 vengolo tolte -----
target=dati[,c("drinks_day")]
covariate=dati[,c(2,4:6)]
covariate=as.matrix(covariate)
library(mctest)
imcdiag(covariate,target) # non funziona
imcdiag(lm1) # funziona 


### Second model ----
# trasformo Y tramite log, tolgo variabili non significative
# tolgo intercetta, non ha senso
hist(dati$drinks_day) #esponenziale negativissima
lm2<-lm(drinks_day~0
        +age+gender
        +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati) 
summary(lm2)

par(mfrow=c(2,2)) 
plot(lm2)
par(mfrow=c(1,1)) 

bptest(lm2) #presenta eteros. serve sistemare l'inferenza.
coeftest(lm2, vcov=vcovHC(lm2)) #infatti suggerisce di togliere gen_health

imcdiag(lm2)


p<-predict(lm1,dati)
plot(p, dati$drinks_day) #brutto

car::ncvTest(lm2) # permane eteroschedasticità

### Modello con solo quantitative ----
lmq<-lm(drinks_day~0+
        +(age*income)+iron, data=dati) 
summary(lmq)

par(mfrow=c(2,2)) 
plot(lmq)
par(mfrow=c(1,1)) 
car::ncvTest(lmq) #con questa versione l'eteros. e ne va se si lavora di più
# nel modello successivo aggiungo un factor
coeftest(lmq, vcov=vcovHC(lmq)) 

### Modello con solo un factor ----
lmq1<-lm(log(drinks_day+1)~0
        +exp(age)+exp(income)+iron+gender, data=dati) 
summary(lmq1)

par(mfrow=c(2,2)) 
plot(lmq1)
par(mfrow=c(1,1)) 
car::ncvTest(lmq1) # ricompare
# riprendere più avanti lo studio del modello con solo quantitativi

### Model with interactions ----
lmi<-lm(drinks_day~0
        +age*education*gender+marital*bmi+iron, data=dati) 
summary(lmi) #inutile e pesante



### Osservazioni influenti? Codice lovaglio -----
# outliers

influencePlot(lm2,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

cooksd <- cooks.distance(lm2)
cooksda=data.frame(cooksd)

# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!

n_used=length(lm2$residuals)
n_used # be careful!!! 

cutoff <- 4/(n_used-length(lm2$coefficients)-2)
cutoff
Noinflu=data.frame(dati[cooksd < cutoff, ])  # influential row numbers
hist(Noinflu$drinks_day)
plot(ecdf(Noinflu$drinks_day))

lminf = lm(drinks_day~0
           +age*education+gender
           +marital
           +bmi
           +gen_health+iron, data=Noinflu)
summary(lminf)

par(mfrow=c(2,2)) 
plot(lminf)
par(mfrow=c(1,1)) 

bptest(lminf) #presenta eteros. serve sistemare l'inferenza.
# migliora il modello

### Distanze di cook valori influenti: versione Pennoni ----
require(faraway)
cook <- cooks.distance(lm1)
halfnorm(cook, ylab = "distanza di Cook") # 1,3
### Punti di leva ----
halfnorm(hatvalues(lm1), main="Punti di leva") #2318,2110

### Modello senza certe variabili -----
lmr<-lm(log(drinks_day+1)~0
        +age+gender+ 
          +education+bmi+marital
        #+insurance+private_insur+medicare+medicaid
        +gen_health+iron, data=dati[,c(-1,-3,-2318,-2110)]) 
summary(lmr)

par(mfrow=c(2,2)) 
plot(lmr)
par(mfrow=c(1,1)) 

anova(lm2,lmr) # non conviene usare l'ultimo modello 

###  Inferenza con stand err robusti: da rivedere ----



coeftest(lm2, vcov=vcovHC(lm2)) 

### Box Cox----
library(MASS)
boxcoxreg1<-boxcox(lminf)
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda # suggerisce trasformata y -0.3434343

dati$drinks_day_modificati<-dati$drinks_day^lambda
Noinflu$drinks_day_modificati<-Noinflu$drinks_day^lambda
lmc<-lm(drinks_day_modificati~0
        +age+education
        +gender
        +marital
        +bmi
        +iron, data=Noinflu) 
summary(lmc) #netto miglioramento, droppo l'interazione, strano

par(mfrow=c(2,2)) 
plot(lmc)
par(mfrow=c(1,1)) 

bptest(lmc) #permane eteros
coeftest(lmc, vcov=vcovHC(lmc)) 
anova(lm2,lmc)

### Modello con trasformate ----
library(gam)
gam1<-gam(drinks_day_modificati~0
          +s(age)+education
          +gender
          +marital
          +s(bmi)
          +gen_health+s(iron), data=Noinflu)
summary(gam1)
par(mfrow=c(2,2)) 
plot(gam1)
par(mfrow=c(1,1)) 
# dubbio su age

lmg<-lm(drinks_day_modificati~0
        +exp(age)+education
        +gender
        +marital
        +bmi
        +gen_health+iron, data=Noinflu) 
summary(lmg) 

par(mfrow=c(2,2)) 
plot(lmg)
par(mfrow=c(1,1)) 

bptest(lmg) #permane eteros
#Inutili trasformate sulle x

anova(lmc,lmg) #conferma inutilità

### model selection ----
library(MASS)
step <- stepAIC(lmc, direction="both")
# abbiamo già il modello migliore. 
#Visti i problemi di eteros., conviene cambiare approccio.

### Riprendo il solo quantativo: ultimo modello ----
# Vediamo con influ che succede
influencePlot(lmqg,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

cooksd2 <- cooks.distance(lmqg)
cooksda=data.frame(cooksd2)

# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!

n_used2=length(lmqg$residuals)
n_used2 
cutoff2 <- 4/(n_used2-length(lmqg$coefficients)-2)
cutoff2
Noinflu2=data.frame(dati[cooksd2 < cutoff2, ])  # influential row numbers

lmqgni<-lm(drinks_day~0
           +(age*income)+(iron), data= Noinflu2)
summary(lmqgni) 

par(mfrow=c(2,2)) 
plot(lmqgni)
par(mfrow=c(1,1)) 

bptest(lmqgni) 


# BC
boxcoxregq<-boxcox(lmqgni)
title("Lambda")
lambdaq=boxcoxregq$x[which.max(boxcoxregq$y)]
lambdaq 

lmqbc<-lm(drinks_day^lambdaq~0
        +(age*income)+iron, data=Noinflu2) 
summary(lmqbc)

par(mfrow=c(2,2)) 
plot(lmqbc)
par(mfrow=c(1,1)) 
car::ncvTest(lmqbc) #eteros, usiamo GAM


# GAM
gamq<-gam(drinks_day^lambdaq~0
        +s(age*income)+s(iron), data=Noinflu2) 
summary(gamq)

library(akima)
par(mfrow=c(2,2)) 
plot(gamq)
par(mfrow=c(1,1)) 

# age*iron exponential/log
# iron natural spline

lmqg<-lm(drinks_day^lambdaq~0
          +log(age*income)+ns(iron), data=Noinflu2) 
summary(lmqg)

par(mfrow=c(2,2)) 
plot(lmqg)
par(mfrow=c(1,1)) 
bptest(lmqg) 

# Controllo ipotesi

#Prima
bptest(lm1)
#Dopo
bptest(lmqg) # robusto



# modello robusto con lmrob
library(robust)
lmrob1<-lmRob(drinks_day^lambdaq~0
                 +log(age*income)+(iron), data=dati)
summary(lmrob1)

par(mfrow=c(2,2)) 
plot(lmrob1)
par(mfrow=c(1,1))
bptest(lmrob1)



### Confronto modello iniziale-finale ----
pf<-predict(lmqg,dati)
par(mfrow=c(1,2)) 
plot(p,dati$drinks_day)
plot(pf, dati$drinks_day)


### logistico: drinkdays >= 52 come soglia critica ----
table(dati$drinks_day); median(dati$drinks_day)
drink_dangerous<-ifelse((dati$drinks_day)>6,1,0); table(drink_dangerous)
glm1<-glm(drink_dangerous~0+
                  log(age*income)+(iron)
          , family="binomial", data=dati)

summary(glm1);glm1$null.deviance
glm1$deviance
drop1(glm1, test="LRT")

exp(glm1$coefficients)

r2=1-(glm1$deviance/glm1$null.deviance) # null dev / resid
r2

library(coefplot)
coefplot(glm1, intercept=FALSE)

# PREDICTION
dati$predicted_p <- predict(glm1, dati, type="response") 
tail(dati$predicted_p)

# predicted target
hist(dati$predicted_p,breaks=20)
# faccio tuning soglia a 0.14 altrimenti mi prevede troppi 1 rispetto alla table di sopra
dati$predicted_y <- ifelse(dati$predicted_p > 0.14,1,0); table(dati$predicted_y)
# 211 uni contro i 202 di sopra

length(drink_dangerous)
length(dati$predicted_y)
cm<-100*(table(observed=drink_dangerous, predicted=dati$predicted_y)/nrow(dati)); cm

accuracy=cm[1,1]+cm[2,2]
accuracy #scadente 



