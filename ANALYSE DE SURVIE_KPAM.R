#installation des packages

#install.packages(c("survival", "survminer"))
library("survival")
library("survminer")
#install.packages("tibble")#gestion des dates
library("tibble")

#utilisation des données disponible dans survival

data(cancer, package="survival")
head(lung)
View(lung)
class(lung)
str(lung)#description des variables
library(labelled)
#Valeurs manquantes
summary(is.na(lung))
#representations des valeurs manquantes
#install.packages("naniar")
library(naniar)
gg_miss_var(lung) 

#calcul du temps de survie
library("tibble")

date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
  )

date_ex
#changeons le format des dates au format dates

library(lubridate)
#install.packages("dplyr")
library("dplyr")
date_ex <-
  date_ex %>% 
  mutate(
    sx_date = ymd(sx_date), 
    last_fup_date = ymd(last_fup_date)
  )

date_ex
##CALCUL DE L'INTERVALLE DE TEMPS

date_ex <-
  date_ex %>% 
  mutate(
    os_yrs = as.duration(sx_date %--% last_fup_date) / dyears(1)
  )

date_ex

##création d'objet de survie
library("survival")

surv_objet<-Surv(lung$time, lung$status)[1:10]
surv_objet

#Nous voyons que le sujet 1 a eu un événement au temps 306 jours, le sujet 2 a eu un événement au temps 455 jours, le sujet 3 a été censuré au temps 1010 jours, etc.
#*******************ESTIMATION DES COURBES DE SURVIE KAPLAN***************

#Courbes de survie selon le sexe
fit <- survfit(Surv(time, status) ~ sex, data = lung)
print(fit)#
# sexe masculin survie median 270 jours

summary(fit)# resumé complet
summary(fit)$table

#MEDIAN ET MOYENNE ESTIMES

print(survfit(Surv(time, status) ~ sex, data = lung),print.rmean = TRUE)
#Les temps de survie médians pour chaque groupe représentent le moment auquel la probabilité de survie, S(t), est de 0,5.
##differentes composantes accessibles

d <- data.frame(time = fit$time,
                n.risk = fit$n.risk,
                n.event = fit$n.event,
                n.censor = fit$n.censor,
                surv = fit$surv,
                upper = fit$upper,
                lower = fit$lower
)
head(d)
summary(fit)$time
summary(fit)$surv  #estimation de kaplan
str(summary(fit))  #plus de détails sur le contenu de la liste
#**********Probabilités de survie à des dates données spécifiques*********
#*
summary(survfit(Surv(time, status) ~ 1, data = lung), times=1000)

#courbe de survie

library("survminer")
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
#interpretations
#estimations de la durée de survie dans un an

summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)
#Le ~ 1dans la survfit()fonction indique que nous estimons le Kaplan-Meier sans aucun regroupement
#Nous constatons que la probabilité de survie de 1 an est de 0,409
#Fonction de Risques cumulatif

ggsurvplot(fit,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           fun = "cumhaz")
#****************COMPARAISON DES COURBES DE SURVIE*********
#*
surv_diff <- survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff
#Le test du log-rank pour la différence de survie donne une valeur p de p = 0,0013, indiquant que les groupes de sexe diffèrent significativement en termes de survie.
#****************MODELE DE COX***************************
#Analyse de régression de Cox univariée

res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox
#summary(res.cox)
#Analyse de régression de Cox multivariée

res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
summary(res.cox)
#Visualisation de la distribution estimée des temps de survie

ggsurvplot(survfit(res.cox),data = lung, color = "#2E9FDF",
           ggtheme = theme_minimal())


