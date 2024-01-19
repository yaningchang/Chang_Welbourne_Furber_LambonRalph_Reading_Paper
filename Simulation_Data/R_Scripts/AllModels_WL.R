### Statistical analyses of Word Length Effects
#
library(languageR)
library(lme4)
library(sjstats)
library(ggpubr)
library(rstatix)
library(sjPlot)
library(ggplot2)
library(QuantPsyc)

rm(list=ls())      # clear all variables in memory

options(scipen=999)

##############################################################
#####      Word Length Effects                           #####
##############################################################
#Read WL 357 word stimuli
word_357 <- read.table('Norms/Wl_357.txt', header=T)
group <- c('Intact', 'PA', 'PD', 'SD')
avg_data_naming_word357_correct_rm3SD_all <- data.frame()
data_naming_word357_correct_rm3SD_all <- data.frame()
word_357_accuracy <- data.frame(0)

for (i in 1:length(group)){
  
  if (group[i]=='Intact') {
    #Intact
    data_naming <- read.table('Intact/intact_naming.txt', header=T)
  }
  
  if (group[i]=='PA') {
    #PA
    times <- seq(26, 30, 1)
    data_naming <- read.table('PA/PA_naming.txt', header=T)
    data_naming <- data_naming[(data_naming$times==times[1] | data_naming$times==times[2] |data_naming$times==times[3] |data_naming$times==times[4] | data_naming$times==times[5]),]#select one recovery time
    colnames(data_naming) <- c('item', 'version', 'times', 'cee', 'correct','correct_dist')
    item_lexical_variable <- read.table('Norms/item_lexical_variables.txt', header=T)
    data_naming <- merge(data_naming, item_lexical_variable, by.x='item', by.y = 'item', all.x=TRUE)
    data_naming$version <- data_naming$version+100
  }
  
  if (group[i]=='PD') {
    #PD
    data_naming <- read.table('PD/PD_naming.txt', header=T)
    colnames(data_naming) <- c('item', 'version', 'times', 'cee', 'correct','correct_dist')
    item_lexical_variable <- read.table('Norms/item_lexical_variables.txt', header=T)
    data_naming <- merge(data_naming, item_lexical_variable, by.x='item', by.y = 'item', all.x=TRUE)
    data_naming$version <- data_naming$version+200
    
  }
  
  if (group[i]=='SD') {
    #SD
    data_naming <- read.table('SD/SD_naming.txt', header=T)
    times <- seq(58, 58, 1)
    data_naming <- data_naming[(data_naming$times==times[1] | data_naming$times==times[2] |data_naming$times==times[3] |data_naming$times==times[4] | data_naming$times==times[5]),]#select one recovery time
    colnames(data_naming) <- c('item', 'version', 'times', 'cee', 'correct','correct_dist')
    item_lexical_variable <- read.table('Norms/item_lexical_variables.txt', header=T)
    data_naming <- merge(data_naming, item_lexical_variable, by.x='item', by.y = 'item', all.x=TRUE)
    data_naming$version <- data_naming$version+300
    
  }

  data_naming_word357 <- merge(word_357, data_naming, by.x='word', by.y='word')
  data_naming_word357_correct <- data_naming_word357[(data_naming_word357$correct==1) | (data_naming_word357$correct==100),]
  word_357_accuracy[i] <- dim(data_naming_word357_correct)[1]/dim(data_naming_word357)[1]
  data_naming_word357_correct$wl.x <- as.factor(data_naming_word357_correct$wl.x)
  data_naming_word357_correct_rm2SD <- data_naming_word357_correct[abs(data_naming_word357_correct$cee-mean(data_naming_word357_correct$cee))<=2*sd(data_naming_word357_correct$cee),]
  avg_data_naming_word357_correct <- aggregate(data_naming_word357_correct, by=list(data_naming_word357_correct$version, data_naming_word357_correct$wl.x), FUN='mean')
  
  #remove outlier by item
  version_mean <- aggregate(data_naming_word357_correct$cee, list(data_naming_word357_correct$version, data_naming_word357_correct$wl.x), FUN='mean')
  version_sd <- aggregate(data_naming_word357_correct$cee, list(data_naming_word357_correct$version, data_naming_word357_correct$wl.x), FUN='sd')
  data_naming_word357_correct_version_mean_sd <- merge(data_naming_word357_correct, version_mean, by.x=c("version", "wl.x"), by.y=c("Group.1", "Group.2"))
  data_naming_word357_correct_version_mean_sd <- merge(data_naming_word357_correct_version_mean_sd, version_sd, by.x=c("version", "wl.x"), by.y=c("Group.1", "Group.2"))
  data_naming_word357_correct_rm3SD <- data_naming_word357_correct_version_mean_sd[abs(data_naming_word357_correct_version_mean_sd$cee-data_naming_word357_correct_version_mean_sd$x.x) <= 3*data_naming_word357_correct_version_mean_sd$x.y,]
  
  avg_data_naming_word357_correct_rm3SD <- aggregate(data_naming_word357_correct_rm3SD, by=list(data_naming_word357_correct_rm3SD$version, data_naming_word357_correct_rm3SD$wl.x), FUN='mean')
  avg_data_naming_word357_correct_rm3SD$wl <- avg_data_naming_word357_correct_rm3SD$wl.y
  avg_data_naming_word357_correct_rm3SD$version <- avg_data_naming_word357_correct_rm3SD$version
  avg_data_naming_word357_correct_rm3SD$s_cee <- scale(avg_data_naming_word357_correct_rm3SD$cee)
  avg_data_naming_word357_correct_rm3SD$group <- group[i]
  
  avg_data_naming_word357_correct_rm3SD_all <- rbind(avg_data_naming_word357_correct_rm3SD_all, subset(avg_data_naming_word357_correct_rm3SD, select=c('version','wl', 'group','cee', 's_cee')))
  
  data_naming_word357_correct_rm3SD$group <- group[i]
  data_naming_word357_correct_rm3SD$wl <- data_naming_word357_correct_rm3SD$wl.y
  data_naming_word357_correct_rm3SD$s_cee <- scale(data_naming_word357_correct_rm3SD$cee)
  data_naming_word357_correct_rm3SD_all <- rbind(data_naming_word357_correct_rm3SD_all, subset(data_naming_word357_correct_rm3SD, select=c('version','wl', 'item','group','cee', 's_cee')))
  
}

res.aov <- anova_test(data = avg_data_naming_word357_correct_rm3SD_all, dv = cee, wid = version, between = group, within = wl)
get_anova_table(res.aov)


##############################
## Normalized Error Scores
##############################
avg_data_naming_word357_correct_rm3SD_all_norm_cee <- avg_data_naming_word357_correct_rm3SD_all
avg_data_naming_word357_correct_rm3SD_all_norm_cee$norm_cee <- 0

id <- c(seq(1, 20, 1), seq(1, 20, 1)+100, seq(1, 20, 1)+200, seq(1, 20, 1)+300 )
for (i in 1:length(id)) {
  wl357_cee <- avg_data_naming_word357_correct_rm3SD_all_norm_cee[avg_data_naming_word357_correct_rm3SD_all_norm_cee$version==id[i],'cee'] 
  avg_data_naming_word357_correct_rm3SD_all_norm_cee[avg_data_naming_word357_correct_rm3SD_all_norm_cee$version==id[i],'norm_cee']  <- wl357_cee/wl357_cee[1] 
}

wl357_norm_cee.aov <- anova_test(data = avg_data_naming_word357_correct_rm3SD_all_norm_cee, dv = norm_cee, wid = version, between = group, within = wl)
get_anova_table(wl357_norm_cee.aov)


