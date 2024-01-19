### ANOVA analyses of Frequency and Consistency and nonwords effects for all models
#
library(languageR)
library(lme4) 
library(tidyr)
library(afex)
library(foreign)
library(stringr)

rm(list=ls())      # clear all variables in memory

options(scipen=999)

#############################################
#### Frequency x Consistency
#############################################
#############################################
#####      Intact Model                ######
#############################################
#### Frequency x Consistency
intact_naming <- read.csv('Intact/intact_freq_con.csv', stringsAsFactors = FALSE)
intact_naming$correct <- intact_naming$correct/100
avg_intact_naming_freq_con <- aggregate(intact_naming$correct, by=list(intact_naming$version, intact_naming$Consistency, intact_naming$Frequency), mean)
colnames(avg_intact_naming_freq_con) <- c('simulation', 'consistency', 'frequency', 'accuracy')
avg_intact_naming_freq_con$simulation <- factor(avg_intact_naming_freq_con$simulation)

intact.aov.bysub.naming.freqcon <- aov(accuracy ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_intact_naming_freq_con)
summary(intact.aov.bysub.naming.freqcon)
print(model.tables(intact.aov.bysub.naming.freqcon,"means"),digits=3)

avg_intact_naming_sd <- aggregate(avg_intact_naming_freq_con$accuracy, by=list(avg_intact_naming_freq_con$consistency, avg_intact_naming_freq_con$frequency), sd)
avg_intact_naming_sd$se <- avg_intact_naming_sd$x/sqrt(20)

########## empirical logit transformation
avg_intact_naming_freq_con$elog <- log((avg_intact_naming_freq_con$accuracy + 0.025) / (1 - avg_intact_naming_freq_con$accuracy + 0.025))
intact.aov.bysub.naming.freqcon.logittrans <- aov(elog ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_intact_naming_freq_con)
summary(intact.aov.bysub.naming.freqcon.logittrans)
print(model.tables(intact.aov.bysub.naming.freqcon.logittrans,"means"),digits=3)

avg_intact_naming_logittrans_sd <- aggregate(avg_intact_naming_freq_con$elog, by=list(avg_intact_naming_freq_con$consistency, avg_intact_naming_freq_con$frequency), sd)
avg_intact_naming_logittrans_sd$se <- avg_intact_naming_logittrans_sd$x/sqrt(20)

#### Nonword
intact_nw <- read.csv('/intact/intact_nw.csv', stringsAsFactors = FALSE)
avg_intact_nw <- aggregate(intact_nw$correct/100, by=list(intact_nw$version), mean)
colnames(avg_intact_nw) <- c('simulation', 'accuracy')
avg_intact_nw$elog <- log((avg_intact_nw$accuracy + 0.025) / (1 - avg_intact_nw$accuracy + 0.025))

#############################################
#####      PA Model                    ######
#############################################
pa_naming_wide <- read.csv('PA/PA_freq_con.csv', stringsAsFactors = FALSE)

#### Frequency x Consistency
pa_naming_wide_freq_con <- pa_naming_wide[,-c(7,8)]
avg_pa_naming_wide_freq_con <- aggregate(pa_naming_wide_freq_con, by=list(pa_naming_wide_freq_con$simulation), mean)
avg_pa_naming_wide_freq_con <- subset(avg_pa_naming_wide_freq_con, select=-c(Group.1, lesion))
avg_pa_naming_wide_freq_con$simulation <- factor(avg_pa_naming_wide_freq_con$simulation)
avg_pa_naming_long_freq_con <- gather(avg_pa_naming_wide_freq_con, condition, measurement, RCH:EXL, factor_key=TRUE)
colnames(avg_pa_naming_long_freq_con) <- c('simulation', 'condition', 'accuracy')
avg_pa_naming_long_freq_con$accuracy <- avg_pa_naming_long_freq_con$accuracy/100
avg_pa_naming_long_freq_con$consistency <- factor(substr(as.character(avg_pa_naming_long_freq_con$condition), 1,2))
avg_pa_naming_long_freq_con$frequency <- factor(substr(as.character(avg_pa_naming_long_freq_con$condition), 3,3))

PA.aov.bysub.naming.freqcon <- aov(accuracy ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_pa_naming_long_freq_con)
summary(PA.aov.bysub.naming.freqcon)

avg_PA_naming_sd <- aggregate(avg_pa_naming_long_freq_con$accuracy, by=list(avg_pa_naming_long_freq_con$consistency, avg_pa_naming_long_freq_con$frequency), sd)
avg_PA_naming_sd$se <- avg_PA_naming_sd$x/sqrt(20)

########## empirical logit transformation
avg_pa_naming_long_freq_con$elog <- log((avg_pa_naming_long_freq_con$accuracy + 0.05) / (1 - avg_pa_naming_long_freq_con$accuracy + 0.05))
PA.aov.bysub.naming.freqcon.logittrans <- aov(elog ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_pa_naming_long_freq_con)
summary(PA.aov.bysub.naming.freqcon.logittrans)
print(model.tables(PA.aov.bysub.naming.freqcon.logittrans,"means"),digits=3)

avg_PA_naming_logittrans_sd <- aggregate(avg_pa_naming_long_freq_con$elog, by=list(avg_pa_naming_long_freq_con$consistency, avg_pa_naming_long_freq_con$frequency), sd)
avg_PA_naming_logittrans_sd$se <- avg_PA_naming_logittrans_sd$x/sqrt(20)

#### Nonword
PA_nw <- read.csv('PA/PA_nw.csv', stringsAsFactors = FALSE)
colnames(PA_nw) <- c('simulation', 'accuracy')
PA_nw$accuracy <- PA_nw$accuracy/100
PA_nw$elog <- log((PA_nw$accuracy + 0.025) / (1 - PA_nw$accuracy + 0.025))

#############################################
#####      PD Model                    ######
#############################################
pd_naming_wide <- read.csv('PD/PD_freq_con.csv', stringsAsFactors = FALSE)

#### Frequency x Consistency
pd_naming_wide_freq_con <- pd_naming_wide[,-c(7,8)]
avg_pd_naming_wide_freq_con <- aggregate(pd_naming_wide_freq_con , by=list(pd_naming_wide_freq_con $simulation), mean)
avg_pd_naming_wide_freq_con <- subset(avg_pd_naming_wide_freq_con , select=-c(Group.1, lesion))
avg_pd_naming_wide_freq_con$simulation <- factor(avg_pd_naming_wide_freq_con$simulation)
avg_pd_naming_long_freq_con <- gather(avg_pd_naming_wide_freq_con, condition, measurement, RCH:EXL, factor_key=TRUE)
colnames(avg_pd_naming_long_freq_con) <- c('simulation', 'condition', 'accuracy')
avg_pd_naming_long_freq_con$accuracy <- avg_pd_naming_long_freq_con$accuracy/100
avg_pd_naming_long_freq_con$consistency <- factor(substr(as.character(avg_pd_naming_long_freq_con$condition), 1,2))
avg_pd_naming_long_freq_con$frequency <- factor(substr(as.character(avg_pd_naming_long_freq_con$condition), 3,3))

PD.aov.bysub.naming.freqcon <- aov(accuracy ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_pd_naming_long_freq_con)
summary(PD.aov.bysub.naming.freqcon)
print(model.tables(PD.aov.bysub.naming.freqcon,"means"),digits=3)

avg_PD_naming_sd <- aggregate(avg_pd_naming_long_freq_con$accuracy, by=list(avg_pd_naming_long_freq_con$consistency, avg_pd_naming_long_freq_con$frequency), sd)
avg_PD_naming_sd$se <- avg_PD_naming_sd$x/sqrt(20)


########## empirical logit transformation
avg_pd_naming_long_freq_con$elog <- log((avg_pd_naming_long_freq_con$accuracy + 0.05) / (1 - avg_pd_naming_long_freq_con$accuracy + 0.05))
PD.aov.bysub.naming.freqcon.logittrans <- aov(elog ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_pd_naming_long_freq_con)
summary(PD.aov.bysub.naming.freqcon.logittrans)
print(model.tables(PD.aov.bysub.naming.freqcon.logittrans,"means"),digits=3)

avg_PD_naming_logittrans_sd <- aggregate(avg_pd_naming_long_freq_con$elog, by=list(avg_pd_naming_long_freq_con$consistency, avg_pd_naming_long_freq_con$frequency), sd)
avg_PD_naming_logittrans_sd$se <- avg_PD_naming_logittrans_sd$x/sqrt(20)

#### Nonword
PD_nw <- read.csv('PD/PD_nw.csv', stringsAsFactors = FALSE)
colnames(PD_nw) <- c('simulation', 'accuracy')
PD_nw$accuracy <- PD_nw$accuracy/100
PD_nw$elog <- log((PD_nw$accuracy + 0.025) / (1 - PD_nw$accuracy + 0.025))

#############################################
#####      SD Model                    ######
#############################################
SD_naming_all <- read.table('SD/SD_naming.txt', header=T)
all_times <- seq(58, 58, 5)
data_lm <- data.frame(times=0, Freq=0, Cons=0, Inter=0)

SD_naming <- SD_naming_all[SD_naming_all$times==all_times,]
  
word <- read.csv('SD/pmsp_coalssemantic.csv', stringsAsFactors = FALSE)
word$id <- seq(1, nrow(word),1)
  
SD_naming_merge <- merge(SD_naming, word, by.x='item', by.y='id')
SD_naming_freq_con <- SD_naming_merge[(SD_naming_merge$type=='EXH' | SD_naming_merge$type=='EXL' | SD_naming_merge$type=='RCH' | SD_naming_merge$type=='RCL'),]
  
avg_SD_naming_freq_con <- aggregate(SD_naming_freq_con$correct, by=list(SD_naming_freq_con$version, SD_naming_freq_con$type), mean)
colnames(avg_SD_naming_freq_con) <- c('simulation', 'condition', 'accuracy')
avg_SD_naming_freq_con$accuracy <- avg_SD_naming_freq_con$accuracy

avg_SD_naming_freq_con$consistency <- factor(substr(as.character(avg_SD_naming_freq_con$condition), 1,2))
avg_SD_naming_freq_con$frequency <- factor(substr(as.character(avg_SD_naming_freq_con$condition), 3,3))
avg_SD_naming_freq_con$simulation <- factor(avg_SD_naming_freq_con$simulation)
  
SD.aov.bysub.naming.freqcon <- aov(accuracy ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_SD_naming_freq_con)
print(summary(SD.aov.bysub.naming.freqcon))
print(model.tables(SD.aov.bysub.naming.freqcon,"means"),digits=3)

########## empirical logit transformation
avg_SD_naming_freq_con$elog <- log((avg_SD_naming_freq_con$accuracy + 0.025) / (1 - avg_SD_naming_freq_con$accuracy + 0.025))
SD.aov.bysub.naming.freqcon.logittrans <- aov(elog ~ frequency*consistency + Error(simulation/(frequency+consistency)), data=avg_SD_naming_freq_con)
summary(SD.aov.bysub.naming.freqcon.logittrans)
print(model.tables(SD.aov.bysub.naming.freqcon.logittrans,"means"),digits=3)

avg_SD_naming_logittrans_sd <- aggregate(avg_SD_naming_freq_con$elog, by=list(avg_SD_naming_freq_con$consistency, avg_SD_naming_freq_con$frequency), sd)
avg_SD_naming_logittrans_sd$se <- avg_SD_naming_logittrans_sd$x/sqrt(20)

#### Nonword
SD_nw_all <- read.table('SD/SD_nw.txt', header=T)
SD_nw <- SD_nw_all[SD_nw_all$times==all_times,]
avg_SD_nw <- aggregate(SD_nw$correct, by=list(SD_nw$version), mean)
colnames(avg_SD_nw) <- c('simulation', 'accuracy')
avg_SD_nw$accuracy <- avg_SD_nw$accuracy
avg_SD_nw$elog <- log((avg_SD_nw$accuracy + 0.025) / (1 - avg_SD_nw$accuracy + 0.025))


#############################################
#  Combined
#############################################
#### Frequency x Consistency
avg_intact_naming_freq_con$type <- 'Intact'
avg_pa_naming_long_freq_con$type <- 'PA'
avg_pd_naming_long_freq_con$type <- 'PD'
avg_SD_naming_freq_con$type <- 'SD'

data_all <- rbind(avg_intact_naming_freq_con, avg_pa_naming_long_freq_con[,-c(2)], avg_pd_naming_long_freq_con[,-c(2)], avg_SD_naming_freq_con[,-c(2)])
data_all$simulation <- factor(data_all$simulation)
data_all$consistency <- factor(data_all$consistency)
data_all$frequency <- factor(data_all$frequency)
data_all$type <- factor(data_all$type)

all.aov.bysub.naming.freqcon <- aov(accuracy ~ frequency*consistency*type + Error(simulation/(frequency+consistency+type)), data=data_all)
summary(all.aov.bysub.naming.freqcon)
print(model.tables(all.aov.bysub.naming.freqcon,"means"),digits=3)

all.aov.bysub.naming.freqcon.logittrans <- aov(elog ~ frequency*consistency*type + Error(simulation/(frequency+consistency+type)), data=data_all)
summary(all.aov.bysub.naming.freqcon.logittrans)
print(model.tables(all.aov.bysub.naming.freqcon.logittrans,"means"),digits=3)


#### Nonword
avg_intact_nw$type <- 'Intact'
PA_nw$type <- 'PA'
PD_nw$type <- 'PD'
avg_SD_nw$type <- 'SD'

nw_all <- rbind(avg_intact_nw, PA_nw, PD_nw, avg_SD_nw)
nw_all$simulation <- factor(nw_all$simulation)

#ANOVA by subject
all.aov.bysub.naming.nw <- aov(accuracy ~ type + Error(simulation/(type)), data=nw_all)
summary(all.aov.bysub.naming.nw)
print(model.tables(all.aov.bysub.naming.nw,"means"),digits=3)

all.aov.bysub.naming.nw.logittrans <- aov(elog ~ type + Error(simulation/(type)), data=nw_all)
summary(all.aov.bysub.naming.nw.logittrans)
print(model.tables(all.aov.bysub.naming.nw.logittrans,"means"),digits=3)
