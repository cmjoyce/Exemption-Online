##set working directory
setwd("~/Documents/Buttenheim Lab/Exemption-Online")

#setup pastecs for descriptives
library(pastecs)

#setup dplyr
library(dplyr)

#setup ggplot2
library(ggplot2)

#setup stringr
library(stringr)

#setup car
library(car)

#setup standardize
library(standardize)

#setup tangram
library(tangram)

#setup ggpubr
library(ggpubr)

#setup psych
library(psych)

#setup plotrix
library(plotrix)

#install tidyverse
library(tidyverse)

##read in baseline dataset
base <- read.csv("Exemption_Trial_Official_Baseline_MAY03_CLEAN.csv")

#read in post data
post <- read.csv("Cleaned data set 2.12.18 CJ.csv")

#recode PACV variables
#10 (PACV 1).  a = 0 b = 2 c = 1
#11 (PACV 2).  a = 2 b = 0 c = 1
#12 (PACV 3).  a = 2 b = 0 c = 1
#13 (PACV 4).  a = 2 b = 0 c = 1
#14 (PACV 5).  a = 2 b = 0 c = 1
base$PACV1 <- recode_factor(base$PACV1, `1` = 0L, `2` = 2L, `3` = 1L)
base$PACV2 <- recode_factor(base$PACV2, `1` = 2L, `2` = 0L, `3` = 1L)
base$PACV3 <- recode_factor(base$PACV3, `1` = 2L, `2` = 0L, `3` = 1L)
base$PACV4 <- recode_factor(base$PACV4, `1` = 2L, `2` = 0L, `3` = 1L)
base$PACV5 <- recode_factor(base$PACV5, `1` = 2L, `2` = 0L, `3` = 1L)

#convert to numbers
base$PACV1 <- as.numeric(as.character(base$PACV1))
base$PACV2 <- as.numeric(as.character(base$PACV2))
base$PACV3 <- as.numeric(as.character(base$PACV3))
base$PACV4 <- as.numeric(as.character(base$PACV4))
base$PACV5 <- as.numeric(as.character(base$PACV5))

#create variable that is sum of PACV scores
base$PACV_sum <- base$PACV1 + base$PACV2 + base$PACV3 + base$PACV4 + base$PACV5

#ANOVA to look at pre-test PACV scores with vaccine history of child and parental intention
#to pursue being up to date
PACVvaxaov <- (lm(PACV_sum ~ VACSTS, data = base))
summary(PACVvaxaov)
anova(PACVvaxaov)

#ANOVA to look at pre-test PACV scores with parental intention to get a nonmedx
PACVnonmedx <- (lm(PACV_sum ~ NONMEDX, data = base))
summary(PACVnonmedx)
anova(PACVnonmedx)

#count number of participants with PACV score > 4
#length(which(base$PACV_sum > 4))

##merge data for only participants who were in RCT
df<- merge(base, post, by = c("workerId"))

#merge data for ALL participants to create master CSV
dfALL <- merge(base, df, by = c("workerId"), all = TRUE)
#write.csv(dfALL, "dfALL.csv")

##merge data for only participants who were in RCT
dfALLCSV<- merge(baseWRONG, dfALL, by = c("workerId"), all = TRUE)

#write.csv(dfALLCSV, "Entire Dataset.csv")

#write.csv(df, "mergedCJ_May2018.csv")

#see how many in each group
length(which(df$arm == 1))
length(which(df$arm == 2))
length(which(df$arm == 3))
length(which(df$arm == 4))

#see how many would like more information about state vaccine requirements
length(which(df$arm== 1 & df$STEX2 == 2))
length(which(df$arm== 2 & df$STEX2 == 2))
length(which(df$arm== 3 & df$STEX2 == 2))
length(which(df$arm== 4 & df$STEX2 == 2))

#see how many would not like more info about state vaccine requirements
length(which(df$arm== 1 & df$STEX2 == 0))
length(which(df$arm== 2 & df$STEX2 == 0))
length(which(df$arm== 3 & df$STEX2 == 0))
length(which(df$arm== 4 & df$STEX2 == 0))


#see how many started
length(which(df$Progress.x == 100))

#see how many of each gender
length(which(df$SEX == 0))
length(which(df$SEX == 2))

#how many over 61
length(which(df$AGE == 61))
length(which(df$AGE == 0))

#average age
mean(df$AGE)

#number of children
mean(df$CHLD1)

#how many less than a high school education
length(which(df$EDUC == 3))

#what state do they live in
length(which(df$STATE == 1))
length(which(df$STATE == 51))

#income 
length(which(df$INC25 == 2))

length(which(base$INC50 == 2))
length(which(base$INC50 == 1))

length(which(base$INC35 == 2))
length(which(base$INC35 == 1))

length(which(base$INC75 == 2))
length(which(base$INC75 == 1))

head(base$INC50)

#see how many had children eligible to start kindergarten
#length(which(base$CHLD1 == 0))

#see how many consented in baseline
#length(which(base$CNSNT == 1))

#Recode Post PACV scores
#recode PACV variables
#10 (PACV 1).  a = 0 b = 2 c = 1
#11 (PACV 2).  a = 2 b = 0 c = 1
#12 (PACV 3).  a = 2 b = 0 c = 1
#13 (PACV 4).  a = 2 b = 0 c = 1
#14 (PACV 5).  a = 2 b = 0 c = 1
df$TPACV1 <- recode_factor(df$TPACV1, `1` = 0L, `2` = 2L, `3` = 1L)
df$TPACV2 <- recode_factor(df$TPACV2, `1` = 2L, `2` = 0L, `3` = 1L)
df$TPACV3 <- recode_factor(df$TPACV3, `1` = 2L, `2` = 0L, `3` = 1L)
df$TPACV4 <- recode_factor(df$TPACV4, `1` = 2L, `2` = 0L, `3` = 1L)
df$TPACV5 <- recode_factor(df$TPACV5, `1` = 2L, `2` = 0L, `3` = 1L)

#convert to numbers
df$TPACV1 <- as.numeric(as.character(df$TPACV1))
df$TPACV2 <- as.numeric(as.character(df$TPACV2))
df$TPACV3 <- as.numeric(as.character(df$TPACV3))
df$TPACV4 <- as.numeric(as.character(df$TPACV4))
df$TPACV5 <- as.numeric(as.character(df$TPACV5))

#create variable that is sum of PACV scores
df$TPACV_sum <- df$TPACV1 + df$TPACV2 + df$TPACV3 + df$TPACV4 + df$TPACV5

#only include participants that received arm
#df1 <- subset(df, !is.na(df$arm))

#Did you complete a task for us previously? Answer = Yes
#length(which(df1$Did.you.complete.a.similar.HIT.for.us.previously. == 1))

#Did you complete a task for us previously? Answer = No
#length(which(df1$Did.you.complete.a.similar.HIT.for.us.previously. == 2))

#Did you complete a task for us previously? Answer = Don't remember
#length(which(df1$Did.you.complete.a.similar.HIT.for.us.previously. == 3))

#Were the steps you took to complete the task same or different? Answer = Same
#length(which(df1$Were.the.steps.you.took.to.complete.the.exemption.request.the.same.or.different.as.last.time.you.completed.this.HIT.==1))

#Were the steps you took to complete the task same or different? Answer = Different
#length(which(df1$Were.the.steps.you.took.to.complete.the.exemption.request.the.same.or.different.as.last.time.you.completed.this.HIT.==2))

#Were the steps you took to complete the task same or different? Answer = Don't Remember
#length(which(df1$Were.the.steps.you.took.to.complete.the.exemption.request.the.same.or.different.as.last.time.you.completed.this.HIT.==3))

#make arm a categorical variable
df$arm <- factor(df$arm)

#see how many in each arm
#see how many in each group
length(which(df$arm == 1))
length(which(df$arm == 2))
length(which(df$arm == 3))
length(which(df$arm == 4))

df$INC10 <- as.numeric(df$INC10)
df$INC15 <- as.numeric(df$INC15)
df$INC20 <- as.numeric(df$INC20)
df$INC25 <- as.numeric(df$INC25)
df$INC35 <- as.numeric(df$INC35)
df$INC50 <- as.numeric(df$INC50)
df$INC75 <- as.numeric(df$INC75)

#create income variable
df$income <- ifelse(df$INC35 == 2 && df$INC20 == 2 && df$INC15 == 2 && df$INC10 == 2,"1", ifelse (df$INC25 == 2 && df$INC20 == 2 && df$INC15 == 2 && df$INC10 == 0,"2", ifelse(df$INC25 == 2 && df$INC20 == 2 && df$INC15 == 0,"3", ifelse (df$INC25 == 2 && df$INC20 == 0,"4", ifelse(df$INC25 == 0 && df$INC35 == 2, "5", ifelse(df$INC25 == 0 && df$INC35 == 0 && df$INC50 == 2, "6", ifelse(df$INC25 == 0 && INC35 == 0 && df$INC50 == 0 && df$INC75 == 2, "7", ifelse(df$INC25 == 0 && df$INC35 == 0 && df$INC50 == 0 && df$INC75 == 0, "8", "88"))))))))

length(which(df$INC50 == 2))
#calculate PACV Change
df$PACV_change <- df$TPACV_sum - df$PACV_sum

#create conjunction fallacy correct variable
df$conjfall <- ifelse(df$CNJF5 < df$CNJF3, "2", "0")

#create risk aversion variable
df$risk <- ifelse(df$RSK < 3, "1", "0" )

#Run ANOVA to look at the interaction between risk and change in PACV
Risk.aov <- (lm(PACV_change ~ arm + risk + arm*risk, data = df))

summary(Risk.aov)
anova(Risk.aov)


#find means by arm + risk aversion category
df %>%
  group_by(arm, risk) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, risk) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#ANOVA for nonmedx
df$nonmedx_cat <- ifelse(df$NONMEDX > 0.439, 1, 0)
Nonmedx_cat.aov <- (lm(PACV_change ~ arm + nonmedx_cat + arm*nonmedx_cat, data = df))
anova(Nonmedx_cat.aov)

#find means by arm + non medical exemption category
df %>%
  group_by(arm, nonmedx_cat) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, nonmedx_cat) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))


#make sure political social  reads as numeric
df$POLSOC <- as.numeric(df$POLSOC)

#political social creation
df$social <- ifelse(df$POLSOC < 4, 1, 0)

#Run ANOVA to look at the interaction between political social and change in PACV
PolSoc.aov <- (lm(PACV_change ~ arm + social + arm*social, data = df))

summary(PolSoc.aov)
anova(PolSoc.aov)

#find means by arm + social  category
df %>%
  group_by(arm, social) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, social) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#Run ANOVA to look at the interaction between parity and change in PACV
Parity.aov <- (lm(PACV_change ~ arm + CHLD1 + arm*CHLD1, data = df))

summary(Parity.aov)
anova(Parity.aov)


#find means by arm + parity category
df %>%
  group_by(arm, CHLD1) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, CHLD1) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#rerun looking just at categories of 1 kid and >1 kid
#first create variable for those with one child and those with more than one child
df$child <- ifelse(df$CHLD1 == 1, 1, 2)
df %>%
    group_by(arm, child) %>%
    summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
    group_by(arm, child) %>%
    summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))


#Optimism Bias variable
#reverse code optimism bias #1_4
df$OPTB1_4_rev <- reverse.code(-1, df$OPTB1_4)

#sum optimism bias 1
df$OPTBsum <- df$OPTB1_1+df$OPTB1_2+df$OPTB1_3+df$OPTB1_4_rev+df$OPTB1_5+df$OPTB1_6

#reverse code optimism #2_4
df$OPTB2_4_rev <- reverse.code(-1, df$OPTB2_4)

#sum optimism bias 2
OPTB2sum <- df$OPTB2_1+df$OPTB2_2+df$OPTB2_3+df$OPTB2_4_rev+df$OPTB2_5+df$OPTB2_6

#find difference between optimism bias
df$OPTB1dif <- df$OPTB1_1 - df$OPTB2_1
df$OPTB2dif <- df$OPTB1_2 - df$OPTB2_2
df$OPTB3dif <- df$OPTB1_3 - df$OPTB2_3
df$OPTB4dif <- df$OPTB1_4_rev - df$OPTB2_4_rev
df$OPTB5dif <- df$OPTB1_5 - df$OPTB2_5
df$OPTB6dif <- df$OPTB1_6 - df$OPTB2_6

#Sum differences in optimism bias
df$OPTBdiff_sum <- df$OPTB1dif + df$OPTB2dif + df$OPTB3dif + df$OPTB4dif + df$OPTB5dif + df$OPTB6dif

#standardize the sum
df$OPTBdiffstand <- scale(df$OPTBdiff_sum)

#take top third of OPTB and make 1/0
df$Optimism <- ifelse(df$OPTBdiffstand > .439, 1, 0)

#Run ANOVA to look at the interaction between optimism bias and change in PACV
Optimism.aov <- (lm(PACV_change ~ arm + Optimism + arm*Optimism, data = df))

summary(Optimism.aov)
anova(Optimism.aov)

#find means by arm + optimism bias category
df %>%
  group_by(arm, Optimism) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, Optimism) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#find how many finsished the study
df %>% 
    group_by(arm) %>%
    count(Progress.x == 100)

df %>%
    group_by(arm) %>%
    count(STEX2 ==2)

df %>%
    group_by(arm) %>%
    count(STEX2 ==0)

#information avoidance
#reverse code information avoidance variables
df$INFAVD5rev <- reverse.code(-1, df$INFAVD5)
df$INFAVD6rev <- reverse.code(-1, df$INFAVD6)
df$INFAVD7rev <- reverse.code(-1, df$INFAVD7)
df$INFAVD8rev <- reverse.code(-1, df$INFAVD8)

#sum information avoidance
df$INFAVDsum <- df$INFAVD1 + df$INFAVD2 + df$INFAVD3 + df$INFAVD4 + df$INFAVD5rev + df$INFAVD6rev + df$INFAVD7rev + df$INFAVD8rev
 
#standardize
df$INFAVDsumstand <- scale(df$INFAVDsum)

#take top third
df$Information <- ifelse(df$INFAVDsumstand > .439, 1, 0)

#Run ANOVA to look at the interaction between information avoidance and change in PACV
Info.aov <- (lm(PACV_change ~ arm + Information + arm*Information, data = df))

summary(Info.aov)
anova(Info.aov)

#find means by arm + infor avoidance category
df %>%
  group_by(arm, Information) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

df %>%
  group_by(arm, Information) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))


#Base Rate Neglect
#base rate neglect, if >= 70 —> “0”, else “2”
df$base <- ifelse(df$BSRN == 70, "0", "2")

#Sunk Cost Fallacy
#sunk cost fallacy. If snkc1 == 1 and snkc2 == 2 —> “2”, else —> “0”
df$sunk <- ifelse(df$SNKC1 == 1 && df$SNKC2 == 2, "2", "0")

#present bias


#regression of PACV scores after intervention
regTPACV <- lm(TPACV_sum ~ arm + PACV_sum, data = df)
summary(regTPACV)

#corrected for F-statistic
PACVF <- lm(TPACV_sum ~ arm, data = df)
summary(PACVF)

#regression of nonmedical exemption
df$TNONMEDX...Percentage <- as.numeric(as.character(df$TNONMEDX...Percentage))
regNONMEDX <- lm(df$TNONMEDX...Percentage ~ arm + df$NONMEDX, data = df)
summary(regNONMEDX)

#corrected for F-statistic
NonmedxF <- lm(TNONMEDX...Percentage ~ arm, data = df)
summary(NonmedxF)

#calculate mean race by arm
race_table1 <- df %>%
  group_by(arm, RACE) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
View(race_table1)

chisq.test(df$arm, df$RACE)
#calculate mean employment status
df %>%
  group_by(arm) %>%
  summarise_at(vars(EMPL), funs(mean(., na.rm=TRUE)))

chisq.test(df$arm, df$EMPL)

empl_table1 <- df %>%
  group_by(arm, EMPL) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

View(empl_table1)

#calculate % married by arm
mrtl_table1 <- df %>%
  group_by(arm, MRTL) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

View(mrtl_table1)

chisq.test(df$arm, df$MRTL)

#calculate % gender by arm
Gender_table1 <- df %>%
  group_by(arm, SEX) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

Gender_table1

chisq.test(df$arm, df$SEX)

#calculate percentage college graduates by arm
educ_table1 <- df %>%
  group_by(arm, EDUC) %>%
  summarise (n = n()) %>%
  mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))

View(educ_table1)

chisq.test(df$arm, df$EDUC)

income_table1 <- df %>%
    group_by(arm, INC25) %>%
    summarise (n = n()) %>%
    mutate(rel.freq = paste0(round(100 * n/sum(n), 0), "%"))
income_table1

#View(educ_table1)
#calculate mean political social 
df %>%
  group_by(arm) %>%
  summarise_at(vars(POLSOC), funs(mean(., na.rm=TRUE)))

#chi square test to see if significantly different
chisq.test(df$arm, df$POLSOC)

#calculate mean political economy
df %>%
  group_by(arm) %>%
  summarise_at(vars(POLECO), funs(mean(., na.rm=TRUE)))

#chi square test to see if significantly different
chisq.test(df$arm, df$POLECO)

#regression of likelihood toget a medical exemption by arm
regMEDX <- lm(df$TMEDX...Percentage~ arm + df$MEDX, data = df)
summary(regMEDX)
MedxF <- lm(df$TMEDX...Percentage ~arm, data = df)
summary(MedxF)

#regression of likelihood to be up to date
regVACSCH <- lm(df$TVACSCHL...Percentage~ arm + df$VACSCHL, data = df)
summary(regVACSCH)

#corrected for F-statistic
VACSCHF <- lm(df$TVACSCHL...Percentage ~ arm, data = df)
summary(VACSCHF)

#only include people who did not complete
dfnon <- subset(post, is.na(post$TMEDX...Percentage))

#calculate change in PACV and one-way ANOVA PACV_change by arm
df$PACV_change <- df$TPACV_sum - df$PACV_sum
PACV.aov <- aov(PACV_change ~ arm, data = df)

summary(PACV.aov)
anova(PACV.aov)
TukeyHSD(PACV.aov)

ggboxplot(df, x = "arm", y = "PACV_change", 
          color = "arm", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("arm1", "arm2", "arm3", "arm4"),
          ylab = "PACV Change", xlab = "Arm")

#summary(glht(PACV.aov, linfct = mcp(arm = "Tukey")))


#average change score by group
df %>%
  group_by(arm) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

#standard deviations
df %>%
  group_by(arm) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#plotmeans(PACV_change ~ arm, data = df, frame = FALSE)

#average PACV score arm 3
df %>%
    filter(arm == 3) %>%
    summarise_at(vars(PACV_change), funs(mean(., na.rm = TRUE)))

#boxplot(PACV_change)
#means.barplot <- qplot(x=arm, y=PACV_change, data=df, geom="bar", stat="identity")
#change.PACV.arm <- split(df$PACV_change, df$arm)
#boxplot(change.PACV.arm, col = "lavender")

 
#average baseline PACV of finishers (all arms)
mean(df$PACV_sum, na.rm = TRUE)
sd(df$PACV_sum, na.rm = TRUE)
dfPACVsum <- data.frame(df$PACV_sum)
names(dfPACVsum) <- c("Completers PACV Sum")


#write.csv(dfPACVsum, "PACV Sum.csv")


#baseline PACV invited but did not start
#create new dataframe with only worker ID and PACV sum score
#find workers who scored >= to 4 on PACV
baseconditional <- mutate(base, PACVConditional = ifelse(PACV_sum > 4, "Yes", "No"))
dfbase <- data.frame(baseconditional$workerId, baseconditional$PACVConditional, baseconditional$PACV_sum)
names(dfbase) <- c("workerId", "PACVConditional", "PACV SUM")

#subset out only those where PACVConditional = Yes (>4)
df1 <- dfbase[ which(dfbase$PACVConditional=='Yes'), ]
#write.csv(df1, "dfinvited.csv")

#remove participants who are in post trial
Nonstarters <- anti_join(df1, df)
mean(Nonstarters$`PACV SUM`)
sd(Nonstarters$`PACV SUM`)

#create group of started and non-completers
dfComplete <- df %>% drop_na(arm)

dfsmall <- data.frame(df$workerId, df$arm, df$PACV_sum)

noncompleters <- anti_join(dfComplete, dfsmall, by = c("workerId"))

dfnon <- df[is.na(df$arm),]
mean(dfnon$PACV_sum)
sd(dfnon$PACV_sum)
dfnonPACV <- data.frame(dfnon$PACV_sum)
names(dfnonPACV) <- c("Non completers PACV sum")
write.csv(dfnonPACV, "dfnonPACV.csv")

#create PACV data frame
dfPACV <- data.frame(df$PACV_sum, Nonstarters$`PACV SUM`, dfnon$PACV_sum)

#read in PACV scores among three groups
PACV <- read.csv("PACV Sum.csv")

t.test(PACV$Completers.PACV.Sum, PACV$Invited.PACV.SUM)
t.test(PACV$Completers.PACV.Sum, PACV$Non.completers.PACV.sum)
t.test(PACV$Invited.PACV.SUM, PACV$Non.completers.PACV.sum)

aovPACV <- aov(PACV$Completers.PACV.Sum ~ PACV$Invited.PACV.SUM + PACV$Non.completers.PACV.sum, data = PACV)
summary(aovPACV)
#separate PACV score into moderate & high
df$PACVcat <- ifelse(df$PACV_sum > 6, 1, 2)
#make arm a categorical variable
df$PACVcat <- factor(df$PACVcat)
#anova including interaction of PACV Category
PACVCat.aov <- (lm(PACV_change ~ arm + PACVcat, data = df))

PACVCat.aov <- (lm(PACV_change ~ arm + PACVcat + arm*PACVcat, data = df))

summary(PACVCat.aov)
anova(PACVCat.aov)

aovPACVCat <- aov(PACV_change ~ arm*PACVcat, data = df)
summary(aovPACVCat)
TukeyHSD(aovPACVCat)


plot(aovPACVCat)
library(ggpubr)
ggline(df, x = "PACVcat", y = "PACV_change", color = "black",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"))

library("ggpubr")
ggboxplot(df, x = "PACVcat", y = "PACV_change", color = "black",
          palette = c("#00AFBB", "#E7B800"))

#means by Category (for figure 2)
df %>%
  group_by(arm, PACVcat) %>%
  summarise_at(vars(PACV_change), funs(mean(., na.rm=TRUE)))

#standard deviation by Category
df %>%
  group_by(arm, PACVcat) %>%
  summarise_at(vars(PACV_change), funs(std.error(., na.rm=TRUE)))

#create change score MEDX
df$MEDX_change <- df$TMEDX...Percentage - df$MEDX

#create change score Nonmedx
df$NONMEDX_change <- df$TNONMEDX...Percentage - df$NONMEDX

#create change score UTD
df$VACSCHL_change <- df$TVACSCHL...Percentage - df$VACSCHL

#mean 3 outcomes by arm
df %>%
  group_by(arm, PACVcat) %>%
  summarise_at(vars(MEDX_change, NONMEDX_change, VACSCHL_change), funs(mean(., na.rm=TRUE)))

#standard error 3 outcomes by arm
df %>%
  group_by(arm) %>%
  summarise_at(vars(MEDX_change, NONMEDX_change, VACSCHL_change), funs(std.error(., na.rm=TRUE)))

#mean 3 outcomes by arm
df %>%
  group_by(arm) %>%
  summarise_at(vars(MEDX_change, NONMEDX_change, VACSCHL_change), funs(mean(., na.rm=TRUE)))

df %>%
    group_by(arm) %>%
    summarise_at(vars(NONMEDX), funs(mean(., na.rm=TRUE)))

df %>%
    group_by(arm) %>%
    summarise_at(vars(TNONMEDX...Percentage), funs(mean(., na.rm=TRUE)))

#standard error 3 outcomes by arm
df %>%
  group_by(arm, PACVcat) %>%
  summarise_at(vars(MEDX_change, NONMEDX_change, VACSCHL_change), funs(std.error(., na.rm=TRUE)))



#ANOVA MEDX
MEDX.aov <- aov(MEDX_change ~ arm, data = df)

MEDXPACV.aov <- aov(MEDX_change ~ arm + PACVcat + arm*PACVcat, data = df)
summary(MEDXPACV.aov)

armPACV  <- df$arm*df$PACVcat

summary(MEDX.aov)

TukeyHSD(MEDX.aov)

summary(glht(MEDX.aov, linfct = mcp(arm = "Tukey")))

# NONMEDX ANOVA

NONMEDX.aov <- aov(NONMEDX_change ~ arm, data = df)

NONMEDXPACV.aov <- aov(NONMEDX_change ~ arm + PACVcat + arm*PACVcat, data = df)
summary(NONMEDXPACV.aov)

summary(NONMEDX.aov)

TukeyHSD(NONMEDX.aov)

summary(glht(NONMEDX.aov, linfct = mcp(arm = "Tukey")))

# UTD ANOVA
VACSCHL.aov <- aov(VACSCHL_change ~ arm, data = df)
VACSCHLPACV.aov <- aov(VACSCHL_change ~ arm + PACVcat + arm*PACVcat, data = df)
summary(VACSCHLPACV.aov)


summary(VACSCHL.aov)

TukeyHSD(VACSCHL.aov)

summary(glht(VACSCHL.aov, linfct = mcp(arm = "Tukey")))




#write.csv(df, "df.csv")


#write dfnon as CSV
#write.csv(dfnon, "dfnon.csv")
