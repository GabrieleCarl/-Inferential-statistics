install.packages("rlang")
library(rlang)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("labstatR")
library(labstatR)
install.packages("moments")
library(moments)
install.packages("devtools")
library(devtools)
devtools::install_github("cardiomoon/webr")
library(webr)
install.packages("rstatix")
library(rstatix)
install.packages("car")
library(car)
install.packages("MASS")
library(MASS) 
install.packages("lmtest")
library(lmtest)
install.packages("useful")
library(useful)
install.packages("knitr")
library(knitr)
install.packages("png")
library(png)
install.packages("ggpubr")
library(ggpubr)
library(boot)
install.packages("olsrr")
library(olsrr)


df <- read.csv("neonati.csv",
               sep = ",",
               stringsAsFactors = T) # Variabili qualitative come fattori
attach(df)

head(df)

# Task 3
colSums(is.na(df)) # nessun valore mancante

x11()
plot(df)

plot(Tipo.parto)
plot(Ospedale)
plot(Sesso)
hist(Lunghezza)
hist(Cranio)
hist(Anni.madre)
hist(N.gravidanze)
hist(Gestazione)
hist(Peso)
hist(Fumatrici)

plot(density(Lunghezza))
plot(density(Cranio))
plot(density(Anni.madre))
plot(density(Peso))

boxplot(Peso)


# Indici di posizione
summary(df) #età madre anomala (2 valori)
# Distribuzione freq
N<-dim(df)[1]
freq.ass.fum<-table(Fumatrici)
freq.rel.fum<-table(Fumatrici)/N
(distr_freq_fum<-cbind(freq.ass.fum,freq.rel.fum))

freq.ass.ngrav<-table(N.gravidanze)
freq.rel.ngrav<-table(N.gravidanze)/N
(distr_freq_ngrav<-cbind(freq.ass.ngrav,freq.rel.ngrav))

# Variabilità
df%>%
  group_by(Fumatrici)%>%
  summarise(media= mean(Peso),
            dev.st=sd(Peso))
df%>%
  group_by(Fumatrici)%>%
  summarise(media= mean(Gestazione),
            dev.st=sd(Gestazione)) 
df%>%
  group_by(Fumatrici)%>%
  summarise(media= mean(Lunghezza),
            dev.st=sd(Lunghezza))
df%>%
  group_by(Fumatrici)%>%
  summarise(media= mean(Cranio),
            dev.st=sd(Cranio))
df%>%
  group_by(Fumatrici)%>%
  summarise(media= mean(Anni.madre),
            dev.st=sd(Anni.madre))
df%>%
  group_by(Tipo.parto)%>%
  summarise(media= mean(Peso),
            dev.st=sd(Peso))
df%>%
  group_by(Tipo.parto)%>%
  summarise(media= mean(Gestazione),
            dev.st=sd(Gestazione))
df%>%
  group_by(Tipo.parto)%>%
  summarise(media= mean(Lunghezza),
            dev.st=sd(Lunghezza))
df%>%
  group_by(Tipo.parto)%>%
  summarise(media= mean(Cranio),
            dev.st=sd(Cranio))
df%>%
  group_by(Tipo.parto)%>%
  summarise(media= mean(Anni.madre),
            dev.st=sd(Anni.madre))
df%>%
  group_by(Sesso)%>%
  summarise(media= mean(Peso),
            dev.sd= sd(Peso))
# Indici di variabilità
# Scarto interquartile
IQR(Anni.madre)
IQR(N.gravidanze)
IQR(Gestazione)
IQR(Peso)
IQR(Lunghezza)
IQR(Cranio)

ggplot(df)+
  geom_boxplot(aes(x=Tipo.parto,
                   y=Peso,
                   fill=Sesso))  #Peso in relazione al tipo di parto e sesso neonato

# Varianza
var(Anni.madre)
var(N.gravidanze)
var(Gestazione)
var(Peso)
var(Lunghezza)
var(Cranio)

# Deviazione standard
sd(Anni.madre)
sd(N.gravidanze)
sd(Gestazione)
sd(Peso)
sd(Lunghezza)
sd(Cranio)

# Coefficiente di variazione
cv(Anni.madre)
cv(N.gravidanze) #Variazione maggiore
cv(Gestazione)
cv(Peso)
cv(Lunghezza)
cv(Cranio)
?cv()
# Indice di Gini
gini.index<- function(x){
  ni=table(x)
  fi=ni/length(x)
  fi2=fi^2
  J=length(table(x))
  gini=1-sum(fi2)
  gini.norm=gini/((J-1)/J)
  return(gini.norm)
  
}

gini.index(Sesso)
gini.index(Fumatrici)
gini.index(Ospedale)

# Indici di forma

skewness(Anni.madre) #Distr Simmetrica
kurtosis(Anni.madre)-3 #Leptocurtica
ggplot()+
  geom_density(aes(x=Anni.madre),col="black",fill="pink")

skewness(N.gravidanze) #Distr Asimmetrica Positiva         
kurtosis(N.gravidanze)-3 #Leptocurtica
ggplot()+
  geom_histogram(aes(x=N.gravidanze),col="black",fill="green")

skewness(Gestazione) #Distr Asimmetrica Negativa
kurtosis(Gestazione)-3 #Leptocurtica
ggplot()+
  geom_histogram(aes(x=Gestazione),col="black",fill="red")

skewness(Peso) #Distr Asimmetrica Negativa
kurtosis(Peso)-3 #Leptocurtica
ggplot()+
  geom_density(aes(x=Peso),col="black",fill="lightblue")

skewness(Lunghezza) #Distr Asimmetrica Negativa
kurtosis(Lunghezza)-3 #Leptocurtica
ggplot()+
  geom_density(aes(x=Lunghezza),col="black",fill="yellow")

skewness(Cranio) #Distr Asimmetrica Negativa
kurtosis(Cranio)-3 #Leptocurtica
ggplot()+
  geom_density(aes(x=Cranio),col="black",fill="brown")

# Task 4
shapiro.test(Peso)

# Test Parametrico
t_test_peso<-t.test(Peso,
       mu = 3300, # mu = Peso medio Bambin Gesù
       conf.level = 0.95,
       alternative = "two.sided")

plot(t_test_peso)


# Test non parametrico
wilcoxTestPeso <- wilcox.test(df$Peso,
                              mu = 3300,
                              conf.level = 0.95,
                              alternative = "two.sided",
                              p.adjust.methods = "bonferroni")
wilcoxTestPeso

shapiro.test(Lunghezza)
t_test_lunghezza<-t.test(Lunghezza,
       mu = 500,    # mu = Lunghezza media Bambin Gesù
       conf.level = 0.95,
       alternative = "two.sided")

plot(t_test_lunghezza)

# Test non parametrico
wilcoxTestLunghezza <- wilcox.test(df$Lunghezza,
                              mu = 500,
                              conf.level = 0.95,
                              alternative = "two.sided",
                              p.adjust.methods = "bonferroni")
wilcoxTestLunghezza

# Task 5
# Test Parametrico
# Peso
shapiro.test(Peso)

df %>%
  group_by(Sesso)%>%
  shapiro_test(Peso)

shapiro.test(subset(df, Sesso == "F")$Peso)
shapiro.test(subset(df, Sesso == "M")$Peso)

var_peso.sesso<-var.test(Peso~Sesso,
                         data = df,
                         alternative = "two.sided")
var_peso.sesso
plot(var_peso.sesso)

boxplot(Peso~Sesso, data = df)
summary(df$Peso[df$Sesso=="F"])
summary(df$Peso[df$Sesso=="M"])
tPesoSesso <- t.test(data = df,
                     Peso~Sesso)
tPesoSesso
plot(tPesoSesso)

# Test non parametrico
wilcoxTestPesoSesso <- wilcox.test(df$Peso~df$Sesso,
                                   conf.level = 0.95,
                                   alternative = "two.sided",
                                   p.adjust.methods = "bonferroni")
                                   
wilcoxTestPesoSesso


# Anni Madre
# Test Parametrico
shapiro.test(Anni.madre)
df %>%
  group_by(Sesso)%>%
  shapiro_test(Anni.madre)
var_anniMadre.sesso<-var.test(Anni.madre~Sesso,
                         data = df,
                         alternative = "two.sided")
var_anniMadre.sesso
plot(var_anniMadre.sesso)
boxplot(Anni.madre~Sesso, data = df)
summary(df$Anni.madre[df$Sesso=="F"])
summary(df$Anni.madre[df$Sesso=="M"])
tAnnimadreSesso <- t.test(data = df,
                          Anni.madre~Sesso)
tAnnimadreSesso
plot(tAnnimadreSesso)

# Test non parametrico
wilcoxTestAnniMadreSesso <- wilcox.test(df$Anni.madre~df$Sesso,
                                        conf.level = 0.95,
                                        alternative = "two.sided",
                                        p.adjust.methods = "bonferroni")
wilcoxTestAnniMadreSesso

# Gestazione
# Test Parametrico
shapiro.test(Gestazione)
df %>%
  group_by(Sesso)%>%
  shapiro_test(Gestazione)
var_gestazione.sesso<-var.test(Gestazione~Sesso,
                              data = df,
                              alternative = "two.sided")
var_gestazione.sesso
plot(var_gestazione.sesso)
boxplot(Gestazione~Sesso, data = df)
summary(df$Gestazione[df$Sesso=="F"])
summary(df$Gestazione[df$Sesso=="M"])
tGestazioneSesso <- t.test(data = df,
                           Gestazione~Sesso)
tGestazioneSesso
plot(tGestazioneSesso)

# Test non parametrico
wilcoxTestGestazioneSesso <- wilcox.test(df$Gestazione~df$Sesso,
                                         conf.level = 0.95,
                                         alternative = "two.sided",
                                         p.adjust.methods = "bonferroni")
wilcoxTestGestazioneSesso

# Lunghezza
# Test Parametrico
shapiro.test(Lunghezza)
df %>%
  group_by(Sesso)%>%
  shapiro_test(Lunghezza)
var_lunghezza.sesso<-var.test(Lunghezza~Sesso,
                               data = df,
                               alternative = "two.sided")
var_lunghezza.sesso
plot(var_lunghezza.sesso)
boxplot(Lunghezza~Sesso, data = df)
summary(df$Lunghezza[df$Sesso=="F"])
summary(df$Lunghezza[df$Sesso=="M"])
tLunghezzaSesso <- t.test(data = df,
                          Lunghezza~Sesso)
tLunghezzaSesso
plot(tLunghezzaSesso)

# Test non parametrico
wilcoxTestLunghezzaSesso <- wilcox.test(df$Lunghezza~df$Sesso,
                                        conf.level = 0.95,
                                        alternative = "two.sided",
                                        p.adjust.methods = "bonferroni")
wilcoxTestLunghezzaSesso

# Cranio
# Test Parametrico
shapiro.test(Cranio)
df %>%
  group_by(Sesso)%>%
  shapiro_test(Cranio)
var_cranio.sesso<-var.test(Cranio~Sesso,
                              data = df,
                              alternative = "two.sided")
var_cranio.sesso
plot(var_cranio.sesso)
boxplot(Cranio~Sesso, data = df)
summary(df$Cranio[df$Sesso=="F"])
summary(df$Cranio[df$Sesso=="M"])
tCranioSesso <- t.test(data = df,
                       Cranio~Sesso)
tCranioSesso
plot(tCranioSesso)

# Test non parametrico
wilcoxTestCranioSesso <- wilcox.test(df$Cranio~df$Sesso,
                                     conf.level = 0.95,
                                     alternative = "two.sided",
                                     p.adjust.methods = "bonferroni")
wilcoxTestCranioSesso

# Numero di gravidanze
# Test Parametrico
shapiro.test(N.gravidanze)
df %>%
  group_by(Sesso)%>%
  shapiro_test(N.gravidanze)
var_gravidanze.sesso<-var.test(N.gravidanze~Sesso,
                           data = df,
                           alternative = "two.sided")
var_gravidanze.sesso
plot(var_gravidanze.sesso)
boxplot(N.gravidanze~Sesso, data = df)
summary(df$N.gravidanze[df$Sesso=="F"])
summary(df$N.gravidanze[df$Sesso=="M"])
tGravidanzeSesso <- t.test(data = df,
                       N.gravidanze~Sesso)
tGravidanzeSesso
plot(tGravidanzeSesso)

# Test non parametrico
wilcoxTestGravidanzeSesso <- wilcox.test(df$N.gravidanze~df$Sesso,
                                     conf.level = 0.95,
                                     alternative = "two.sided",
                                     p.adjust.methods = "bonferroni")
wilcoxTestGravidanzeSesso

# Task 5 Test Non-Parametrici (se non si rispettano le assunzioni del t-test)
pairwise.wilcox.test(df$Peso,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")

pairwise.wilcox.test(df$Anni.madre,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")

pairwise.wilcox.test(df$N.gravidanze,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")

pairwise.wilcox.test(df$Gestazione,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")

pairwise.wilcox.test(df$Lunghezza,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")

pairwise.wilcox.test(df$Cranio,
                     df$Sesso,
                     paired = F,
                     pool.sd = T,
                     p.adjust.method = "bonferroni")


# Task 6 Calcolo la percentuale di parto Ces in ogni ospedale
partiTotali <- df%>%
  group_by(Ospedale)%>%
  count(Ospedale)


partiCesEnat <- df%>%
  group_by(Ospedale, Tipo.parto)%>%
  count(Ospedale)

df%>%
  group_by(Ospedale, Tipo.parto=="Ces")%>%
  count(Ospedale)

  


partiTuttiOspedali<-partoCesareo%>% #doesn't work -> partoCesareo?
  group_by(Ospedale)%>%
  summarise(across(where(is.numeric),sum))

osp1Ces <- 242
osp2Ces <- 254
osp3Ces <- 232

osp1Nascite <- 816
osp2Nascite <- 849
osp3Nascite <- 835

osp1PercenCes <- osp1Ces/osp1Nascite
osp2PercenCes <- osp2Ces/osp2Nascite
osp3PercenCes <- osp3Ces/osp3Nascite
# L'ospedale con la percentuale di parti ces più alta è:
# - osp2 = 0.299 > osp1 = 0.296 > osp3 = 0.277

differenzeParti <- as.matrix(rbind(c(242,574),c(254,595),c(232,603)))
dimnames(differenzeParti) <- list(Ospedale = c("osp1", "osp2", "osp3"),
                                  Tipologia = c("Ces", "Nat"))

ggpubr::ggballoonplot(data = as.data.frame(differenzeParti),
                      fill = "red",
                      rotate.x.text = T)
differenzeParti

# Calcolo frequenze attese
differenzePartiAttese <- differenzeParti
margin.table(differenzeParti, 1)
margin.table(differenzeParti, 2)
n = margin.table(differenzeParti)

for(i in 1:nrow(differenzeParti)){
  for(j in 1:ncol(differenzeParti)){
    differenzePartiAttese[i,j] <- (margin.table(differenzeParti, 1)[i] * margin.table(differenzeParti, 2)[j])/n
  }
}


testTipoParto <- chisq.test(differenzeParti)
testTipoParto

testTipoParto$expected

plot(testTipoParto)

# H0 chi quadro = 0 (indipendenza tra le variabili)
# H1 chi quadro > 0 (chi quadro < 0 impossibile, quindi dipendenza tra le variabili)

# p-value > alpha quindi non si rifiuta H0, le due variabili sono tra loro indipendenti

diffParti_percent <- as.table(rbind(c(osp1PercenCes,osp2PercenCes,osp3PercenCes)))
dimnames(diffParti_percent) <- list(RappParti = c("ces/tot"),
                                    Ospedale = c("osp1", "osp2", "osp3"))
diffParti_percent
testPercentParti <- chisq.test(diffParti_percent)
testPercentParti

# H0 chi quadro = 0 (indipendenza tra le variabili)
# H1 chi quadro > 0 (chi quadro < 0 impossibile, quindi dipendenza tra le variabili)

# p-value > alpha quindi non si rifiuta H0, le due variabili sono tra loro indipendenti


# Analisi Multidimensionale

# Eliminare Valori anomali Età Madre df

newDf <- df%>%
  filter(Anni.madre > 2)

# Task 1
plot(newDf$Anni.madre,newDf$Peso, pch=20)
# Covarianza
cov(newDf$Anni.madre,newDf$Peso) # Discordanza tra variabili
# Indice di correlazione di lineare di Bravis-Pearson
cor(newDf$Anni.madre,newDf$Peso) # Bassa correlazione lineare negativa
plot(newDf$Anni.madre,newDf$Peso)

plot(newDf$N.gravidanze,newDf$Peso)
cor(newDf$N.gravidanze,newDf$Peso) # Assenza di correlazione lineare

plot(newDf$Gestazione,newDf$Peso)
cor(newDf$Gestazione,newDf$Peso) # Media correlazione lineare positiva

plot(newDf$Lunghezza,newDf$Peso)
cor(newDf$Lunghezza, newDf$Peso) # Alta correlazione lineare positiva

plot(newDf$Cranio,newDf$Peso)
cor(newDf$Cranio,newDf$Peso) # Alta correlazione lineare positiva



# Task 2

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

x11()
pairs(newDf, 
      upper.panel = panel.smooth, 
      lower.panel = panel.cor )

# Divisione del dataframe in Train(80%) e Test(20%)
set.seed(123)
trainIndex <- sample(x = 1:nrow(newDf), 
                   replace = F, 
                   size = 0.8*nrow(newDf))
train_df <- newDf[trainIndex, ]
test_df <- newDf[-trainIndex, ]


mod1 <- lm(Peso ~ ., data = train_df)
summary(mod1)
plot(mod1)

# Task 3
# stepwise backward selection

mod2 <- update(mod1,~. - Anni.madre)
summary(mod2)

mod3 <- update(mod2,~. -Ospedale)
summary(mod3)

mod4 <- update(mod3,~. -Fumatrici)
summary(mod4)

mod5 <- update(mod4,~. +I(Gestazione^2))
summary(mod5)

mod6 <- lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
             Tipo.parto + Sesso + Cranio:Lunghezza, data = train_df)
summary(mod6)

mod7 <- lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
             Tipo.parto + Sesso + Cranio:Gestazione, data = train_df)
summary(mod7)

mod8 <- lm(Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
             Tipo.parto + Sesso + Gestazione:Lunghezza, data = train_df)
summary(mod8)

mod9 <- update(mod4,~. -Tipo.parto)
summary(mod9)

# Anova

anova(mod9, mod5)
anova(mod9, mod6)
anova(mod9, mod7)
anova(mod9, mod8)

# AIC BIC
AIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9)
BIC(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9)

# Multicollinearità
vif(mod9)
vif(mod7)
vif(modAIC)
vif(modBIC)
vif(mod4)

# stepAIC selection

n.TrainDf <- nrow(train_df)

modAIC <- stepAIC(mod1,
        direction = "both",
        k=2)
summary(modAIC)

modBIC <- stepAIC(mod1,
                  direction = "both",
                  k=log(n.TrainDf))
summary(modBIC)

# Task 5
par(mfrow=c(2,2))
plot(mod9)

# Leverage
#mod9
lev <- hatvalues(mod9)
plot(lev)
# modBIC
levBIC <- hatvalues(modBIC)
plot(levBIC)
# modAIC
levAIC <- hatvalues(modAIC)
plot(levAIC)
# mod4
levmod4 <- hatvalues(mod4)
plot(levmod4)

# idendificazione valore soglia per i valori di leva
# mod9
p = sum(lev)
soglia = 2* p/n.TrainDf
abline(h=soglia,col=2)
lev[lev>soglia]

# modBIC
pBIC = sum(levBIC)
sogliaBIC = 2* pBIC/n.TrainDf
abline(h=sogliaBIC,col=2)
levBIC[levBIC>sogliaBIC]

# modAIC
pAIC = sum(levAIC)
sogliaAIC = 2* pAIC/n.TrainDf
abline(h=sogliaAIC,col=2)
levAIC[levAIC>sogliaAIC]

# mod4
pmod4 = sum(levmod4)
sogliamod4 = 2* pmod4/n.TrainDf
abline(h=sogliamod4,col=2)
levmod4[levmod4>sogliamod4]

# identificazione outliers
# mod9
plot(rstudent(mod9))
abline(h=c(-2,2), col=2)
outlierTest(mod9)

# modBIC
plot(rstudent(modBIC))
abline(h=c(-2,2), col=2)
outlierTest(modBIC)

# modAIC
plot(rstudent(modAIC))
abline(h=c(-2,2), col=2)
outlierTest(modAIC)

# mod4
plot(rstudent(mod4))
abline(h=c(-2,2), col=2)
outlierTest(mod4)

# Distanza di cook
# mod9
cook <- cooks.distance(mod9)
plot(cook)
max(cook)

# modBIC
cookBIC <- cooks.distance(modBIC)
plot(cookBIC)
max(cookBIC)

# modAIC
cookAIC <- cooks.distance(modAIC)
plot(cookAIC)
max(cookAIC)

# mod4
cookmod4 <- cooks.distance(mod4)
plot(cookmod4)
max(cookmod4)

# Test sui residui
# mod9
bptest(mod9)
dwtest(mod9)

shapiro.test(residuals(mod9))
plot(density(residuals(mod9)))

# modBIC
bptest(modBIC)
dwtest(modBIC)

shapiro.test(residuals(modBIC))
plot(density(residuals(modBIC)))

# modAIC
bptest(modAIC)
dwtest(modAIC)

shapiro.test(residuals(modAIC))
plot(density(residuals(modAIC)))

# mod4
bptest(mod4)
dwtest(mod4)

shapiro.test(residuals(mod4))
plot(density(residuals(mod4)))

# Task 6

# IMPORTANTE (test error)

# Il modello serve a fare previsioni future:

# per scegliere il modello migliore devo confrontare il test error dei modelli
# sul dataframe test_df, in quanto solo così posso determinare la bontà del 
# modello sulla capacità di fare previsioni future (dati in test_df non presenti in train_df)
# In pratica, ci sarà un modello che stimerà meglio degli altri il passato (train_df)
# ed uno che stimerà meglio i dati futuri (test_df)

# Esistono due modi per fare questo tipo di misura(commenti sopra):
#
# 1 Metodi indiretti:
#   - Applicano opportune correzioni al training error(RSS,R^2):
#        - AIC (misura dell'errore, più è basso meglio è)
#        - BIC (misura dell'errore, più è basso meglio è)
#        - CP (Cp di Mallows) (misura dell'errore, più è basso meglio è)
#        - Adjusted R^2 (è una misura della bontà del modello, meglio il più alto)
#
# 2 Metodi diretti
#   - Utilizzano delle procedure apposite:
#        - Validation set: 
#              - MSE
#        - Leave one out cross validation (LOOCV)
#                 - Limite risorse computazionali
#                 - Basso Bias Alta Varianza
#        - K-fold cross-validation
#                 - Il più diffuso
#                 - Alternativa a LOOCV perchè riduce il carico computazionale
#                 - Controlla l'equilibrio tra Bias e Varianza con il parametro k:
#                       - Tanto più k è grande (tende a n = nrows(df)) Basso Bias e Alta Varianza 
#                       - Tanto più k è basso (tende a n = nrows(df)/2, ovvero la metà del dataframe) Alto Bias e Bassa Varianza
#                       - Tipicamente, k=5 o k=10 è un buon punto di equilibrio Bias Varianza (considerazioni empiriche)
#         Importante -> - Il k visto sopra indica in quante parti va diviso il dataframe


previsioniDftest <- predict(mod9, test_df)

test_df$Previsione <- previsioniDftest

# Calcolo MSE
MSE <- mean((test_df$Peso - test_df$Previsione)^2)

# Calcolo RMSE
RMSE <- sqrt(MSE)



# Task 7

# La previsione va fatta utilizzando il modello addestrato con tutto il dataframe
# non solo con train_df

mod10 <- lm(formula = Peso ~ N.gravidanze + Gestazione + Lunghezza + Cranio + 
              Sesso, data = newDf)
summary(mod10)




predict_df_task7 <- data.frame(N.gravidanze = 3, 
                     Gestazione = 39,
                     Lunghezza = mean(newDf$Lunghezza),
                     Cranio = mean(newDf$Cranio),
                     Sesso = "F")

predictTask7 <- predict(mod10, predict_df_task7)
predictTask7







# Task 8

modplot3d <- update(mod10,~. - N.gravidanze - Sesso - Gestazione)
summary(modplot3d)

car::scatter3d(Peso ~ Lunghezza + Cranio)


ggplot(data = newDf)+
  geom_point(aes(x=Lunghezza,
                 y=Peso,
                 col= Sesso))+
  geom_smooth(aes(x=Lunghezza,
                  y=Peso,
                  col= Sesso), se=F, method = "lm")

# GLM su train_df

# Modello di GLM Gaussiana (variabile quantitativa continua e simmetrica)
# Link function:Log

# stepwise backward selection

modGLM_all <- glm(Peso~., data = train_df, family = gaussian(link = "log"))
summary(modGLM_all)

modGLM_2 <- update(modGLM_all, ~. - Anni.madre)
summary(modGLM_2)

modGLM_3 <- update(modGLM_2,~. - Fumatrici)
summary(modGLM_3)

modGLM_4 <- update(modGLM_3,~. - Ospedale)
summary(modGLM_4)

modGLM_5 <- update(modGLM_4,~. - Tipo.parto)
summary(modGLM_5)



# Test error
# Metodi indiretti

AIC( modGLM_all, modGLM_2, modGLM_3, modGLM_4, modGLM_5)
BIC( modGLM_all, modGLM_2, modGLM_3, modGLM_4, modGLM_5)

# R^2 and Adj R^2 glm models
R2_modGLM_all <- 1 - (modGLM_all$deviance/modGLM_all$null.deviance)
R2_adj_modGLM_all <- 1-((n.TrainDf-1)/(n.TrainDf-9-1)*(modGLM_all$deviance/modGLM_all$null.deviance))

R2_modGLM_2 <- 1 - (modGLM_2$deviance/modGLM_2$null.deviance)
R2_adj_modGLM_2 <- 1-((n.TrainDf-1)/(n.TrainDf-8-1)*(modGLM_2$deviance/modGLM_2$null.deviance))

R2_modGLM_3 <- 1 - (modGLM_3$deviance/modGLM_3$null.deviance)
R2_adj_modGLM_3 <- 1-((n.TrainDf-1)/(n.TrainDf-7-1)*(modGLM_3$deviance/modGLM_3$null.deviance))

R2_modGLM_4 <- 1 - (modGLM_4$deviance/modGLM_4$null.deviance)
R2_adj_modGLM_4 <- 1-((n.TrainDf-1)/(n.TrainDf-6-1)*(modGLM_4$deviance/modGLM_4$null.deviance))

R2_modGLM_5 <- 1 - (modGLM_5$deviance/modGLM_5$null.deviance)
R2_adj_modGLM_5 <- 1-((n.TrainDf-1)/(n.TrainDf-5-1)*(modGLM_5$deviance/modGLM_5$null.deviance))


# Actual best model glm()

# For AIC = modGLM_3
# For BIC = modGLM_5
# For Adj R^2 = modGLM_3 but modGLM_4 (the last simpler)
# ------------------------------------------------------------


# Nuovi modelli di ML utilizzando tutto il dataframe per addestrarli

# GLM su newDf (quello senza anni madre 0 e 1)

# Modello di GLM Gaussiana (variabile quantitativa continua e simmetrica)
# Link function:Log
# stepwise backward selection

fit_all <- glm(Peso~., data = newDf, family = gaussian(link = "log"))
summary(fit_all)

fit_2 <- update(fit_all, ~. - Anni.madre)
summary(fit_2)

fit_3 <- update(fit_2, ~. - Fumatrici)
summary(fit_3)

fit_4 <- update(fit_3, ~. - Ospedale)
summary(fit_4)

fit_5 <- update(fit_4, ~. - Tipo.parto)
summary(fit_5)

# Test error
# Metodi diretti

# K-Fold Cross Validation 
# MSE
set.seed(456)

boot::cv.glm(newDf, fit_all, K = 5)$delta[1]
boot::cv.glm(newDf, fit_2, K = 5)$delta[1]
boot::cv.glm(newDf, fit_3, K = 5)$delta[1]
boot::cv.glm(newDf, fit_4, K = 5)$delta[1]
boot::cv.glm(newDf, fit_5, K = 5)$delta[1]

# AIC BIC
AIC( fit_all, fit_2, fit_3, fit_4, fit_5)
BIC( fit_all, fit_2, fit_3, fit_4, fit_5)












# ---------------------------------------------------------------------------------
# Per saggiare l'ipotesi delle nascite premature usare dyplr, dividere il dataset
# tra fumatrici e non e confrontare tramite test statistici le medie di 
# settimane di gestazione 
# ---------------------------------------------------------------------------------------

sessionInfo()




