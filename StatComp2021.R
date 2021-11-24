setwd("C:/Users/Gabriele/Documents/GitHub/StatComp2021")
load("StatComp2021.RData")

# Dati: vogliamo capire cosa spiega la frequenza di drinks_day ----
dati<-read.csv("https://raw.githubusercontent.com/F041/StatComp2021/main/Merged_Unique_Names_V2.csv",
               sep=",", dec = ".",  
               stringsAsFactors=TRUE, na.strings=c("NA","NaN"))
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
library(dplyr)
options(scipen=999)

### Descriptives ----
summary(dati) # ordine differente, vedo gli NAs con più fatica
descr<-skim(dati) #non userò le covariate con più missing >2000
descr
# non user num colonna: 5,6,10:14, 16, 18:24 ....

hist(dati$drinks_day) #di sicuro non una normale

### Cambio proprietà dati: tanti factor non riconosciuti come tali -----
#personal
dati$gender<-dati$gender%>%as.factor
dati$education<-dati$education%>%as.factor
dati$marital<-dati$marital%>%as.factor
dati$gen_health<-dati$gen_health%>%as.factor
dati$income<-dati$income%>%as.factor
dati$race<-dati$race%>%as.factor
dati$smoker<-dati$smoker%>%as.factor
dati$diabetes<-dati$diabetes%>%as.factor
dati$cancer<-dati$cancer%>%as.factor
dati$depression<-dati$depression%>%as.factor
dati$hypertension<-dati$hypertension%>%as.factor


#family
dati$household_size<-dati$household_size%>%as.factor
dati$fam_savings<-dati$fam_savings%>%as.factor

#assicuraziomi
dati$insurance<-dati$insurance%>%as.factor
dati$private_insur<-dati$private_insur%>%as.factor
dati$medicare<-dati$medicare%>%as.factor
dati$medicaid<-dati$medicaid%>%as.factor
dati$military_insur<-dati$military_insur%>%as.factor
dati$no_insurance<-dati$no_insurance%>%as.factor


### Divisione tra continue e qualitative ----

numeric <- dati%>% dplyr::select_if(is.numeric)
colnames(numeric)
skim(numeric)
numeric=numeric[,-c(1,3,4,5,6,7,8,9)] #tolgo factor
colnames(numeric)
skim(numeric) #tolgo colonna c(-2,-(7:8),-(40:41),-43), troppi NA
numeric=numeric[,c(-2,-(7:8),-(40:41),-43)];skim(numeric)

fac <- dati%>% dplyr::select_if(is.factor)
colnames(fac)
fac=fac[,-c(1:2)]

education<-fac$education
merge<-cbind(numeric,education); skim(merge)

# collinearitità
# sia per x continue (plot, tol, vif), sia per x qualitative (chi quadri)
y<- (numeric["drinks_day"]); head(y); is.atomic(y)
X<-numeric; is.atomic(X)
X<-as.matrix(X); X; is.atomic(X)
imcdiag(X,y, na.rm = TRUE) #non funziona

library(corrgram)
corrgram(numeric, use = "complete.obs", lower.panel = panel.cor, cex=1, cex.labels = 1)
# bmi, altezzza e peso risultano sicuramente collineari, a causa della formula

### Modelli quantiativi ----
## First model ---
lm = lm(drinks_day ~ ., data=numeric)
summary(lm)
par(mfrow=c(2,2)) 
plot(lm)
par(mfrow=c(1,1)) 


oeftest(lm, vcov=vcovHC(lm)) #si può droppare molto
p<-predict(lm,numeric) #serve dopo

## Second model ---
lm2 = lm(drinks_day ~0+ age+ s_cotinine +wbc +hgb   +t_protein     
         +t_chol  +bmi  +cr   , data=numeric)
summary(lm2)
bptest(lm2)
coeftest(lm2, vcov=vcovHC(lm2)) # droppo t_protein  e cr          

## Third model --
lm3 = lm(drinks_day ~0+ age+ s_cotinine +wbc 
         +hgb        +t_chol    , data=numeric)
summary(lm3)
bptest(lm3) # ancora eteros.
coeftest(lm3, vcov=vcovHC(lm3)) 

## Third model senza age --
lm3na = lm(drinks_day ~0+  s_cotinine +wbc 
           +hgb        +t_chol    , data=numeric)
summary(lm3na)
par(mfrow=c(2,2)) 
plot(lm3na)
par(mfrow=c(1,1)) 
bptest(lm3na) # age dà problemi. Togliendolo scompare eteros.
coeftest(lm3na, vcov=vcovHC(lm3na)) 

imcdiag(lm3na)#nessun problema

## Valori inf e modello 
influencePlot(lm3na,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
cooksd <- cooks.distance(lm3na)
cooksda=data.frame(cooksd)
# cutoff of cookD  4/(n-k).. NB n should be n used in the model!!!
n_used=length(lm3na$residuals)
n_used # 
cutoff <- 4/(n_used-length(lm3na$coefficients)-2)
cutoff
Noinflu=data.frame(numeric[cooksd < cutoff, ])  # influential row numbers

hist(Noinflu$drinks_day) # ancora esponenziale negativa
plot(ecdf(Noinflu$drinks_day)) # almeno non abbiamo outlier e >15

lminf = lm(drinks_day ~0+  s_cotinine +wbc 
           +hgb        +t_chol    , data=Noinflu)
summary(lminf) #migliora
#ma

par(mfrow=c(2,2)) 
plot(lminf)
par(mfrow=c(1,1)) 

bptest(lminf) # viene fuori eteros, non conviene

## Box Cox---
boxcoxreg1<-boxcox(lminf) 
title("Lambda")
lambda=boxcoxreg1$x[which.max(boxcoxreg1$y)]
lambda # suggerisce trasformata y -0.303
numeric$drinks_day_modificati<-numeric$drinks_day^lambda
Noinflu$drinks_day_modificati<-Noinflu$drinks_day^lambda
lmc<-lm(drinks_day_modificati ~0+  s_cotinine +wbc 
        +hgb        +t_chol    , data=Noinflu) 
summary(lmc) #netto miglioramento

par(mfrow=c(2,2)) 
plot(lmc)
par(mfrow=c(1,1)) 

bptest(lmc) #permane eteros
coeftest(lmc, vcov=vcovHC(lmc))


## Modello con trasformate ---
library(gam)
gam1<-gam(drinks_day~0+ + s(s_cotinine) +s(wbc) 
          +s(hgb)        +s(t_chol)    , data=numeric)
summary(gam1)
par(mfrow=c(2,2)) 
plot(gam1)
par(mfrow=c(1,1)) 
# s_cotinine: log
# hgb: parabola

lmg<-lm(drinks_day~0 
        + log(s_cotinine) 
        + (wbc) 
        +I(hgb^2) +t_chol, data=Noinflu) 
summary(lmg)

par(mfrow=c(2,2)) 
plot(lmg)
par(mfrow=c(1,1)) 

drop1(lmg)

bptest(lmg) #torna eteros.

## Quarto modello con trasformata log sulla y causa sua distr --
lm3nay = lm(log(drinks_day+1) ~0+  s_cotinine +wbc 
            +hgb        +t_chol    , data=numeric)
summary(lm3nay)
par(mfrow=c(2,2)) 
plot(lm3nay)
par(mfrow=c(1,1)) 
bptest(lm3nay) # eteros. 
coeftest(lm3nay, vcov=vcovHC(lm3na)) 

## Quinto modello con education ---
lm5 = lm(drinks_day ~0+  s_cotinine +wbc 
         +hgb        +t_chol  +education  , data=merge)
summary(lm5)
par(mfrow=c(2,2)) 
plot(lm5)
par(mfrow=c(1,1)) 
bptest(lm5)
coeftest(lm5, vcov=vcovHC(lm3na)) 

## Sesto modello con robust base
library(robustbase) 
lmrobfit <- lmrob(drinks_day ~0+  s_cotinine +wbc 
                  +hgb        +t_chol    , data=numeric) 
summary(lmrobfit) 

### Riepilogo test ----
bptest(lm)
bptest(lm3na) #robusto anche se spiega meno della metà 

### Confronto modello iniziale-finale ----
pf<-predict(lm3na,numeric)
par(mfrow=c(1,2)) 
plot(p,numeric$drinks_day)
#abline(a = 0, b = 1, col = "red")
plot(pf, numeric$drinks_day)
#abline(a = 0, b = 1, col = "red")

### Logistico----
table(numeric$drinks_day); median(numeric$drinks_day)
# drinkdays >6  come soglia critica altrimenti serve a nulla
drink_dangerous<-ifelse((numeric$drinks_day)>6,1,0); table(drink_dangerous)
glm1<-glm(drink_dangerous~ s_cotinine +wbc 
          +hgb        +t_chol  +education 
          , family="binomial", data=merge)

summary(glm1);glm1$null.deviance
glm1$deviance
drop1(glm1, test="LRT") 
#droppo  s_cotinine, la non significativa
# per dettagli: https://rstudio-pubs-static.s3.amazonaws.com/2899_a9129debf6bd47d2a0501de9c0dc583d.html

glm2<-glm(drink_dangerous~ wbc 
          +t_chol   +education + hgb  
          , family="binomial", data=merge)
summary(glm2)
drop1(glm2, test="LRT") #ok

exp(glm2$coefficients)

r2=1-(glm2$deviance/glm2$null.deviance) # null dev / resid
r2 #quasi nullo

library(coefplot)
coefplot(glm2, intercept=FALSE)

# PREDICTION
merge$predicted_p <- predict(glm2, merge, type="response") 
tail(merge$predicted_p)

# predicted target
hist(merge$predicted_p)
# faccio tuning soglia a 0.1
merge$predicted_y <- ifelse(merge$predicted_p > 0.1,1,0); table(merge$predicted_y)
# 424 uni contro i 202 di sopra

length(drink_dangerous)
length(merge$predicted_y)
cm<-100*(table(observed=drink_dangerous, predicted=merge$predicted_y)/nrow(merge)); cm

accuracy=cm[1,1]+cm[2,2]
accuracy #scadente 
