##set working directory
setwd("~/Documents/Buttenheim Lab/Online Exemption")

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

##read in baseline dataset
base <- read.csv("Exemption_Trial_Official_Baseline_MAY03_CLEAN.csv")

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

#count number of participants with PACV score > 4
#length(which(base$PACV_sum > 4))

#find workers who scored >= to 4 on PACV
#baseconditional <- mutate(base, PACVConditional = ifelse(PACV_sum > 4, "Yes", "No"))

#create new dataframe with only worker ID and PACV sum score
#dfbase <- data.frame(baseconditional$workerId, baseconditional$PACVConditional)
#names(dfbase) <- c("workerId", "PACVConditional")

#subset out only those where PACVConditional = Yes (>4)
#df1 <- dfbase[ which(dfbase$PACVConditional=='Yes'), ]

#check that the numbers match
#nrow(df1)

#write as a csv
#write.csv(df1, "Eligible Worker IDs.csv")

##read in post data
post <- read.csv("Exemption_Trial_Official_Trial_APRIL28.csv")

##merge both for ALL participants based on workerID as is only shared ID b/w datasets
dfALL <- merge(base, post, by = c("workerId"), all = TRUE)

##merge data for only participants who were in RCT
dfmerge<- merge(base, post, by = c("workerId"))

#unmerge
dfALL[!duplicated(dfALL), ]

#remove rows without workerID (first 22 rows were preview, do not have an ID)
dfRCT <- dfmerge[23:485, ]

length(which(dfRCT$PACV_sum == 0))

#add arms 1, 2, 3, and 4 variable
#for arm 1: if they typed "I hereby agree to and sign this statement"
#for arm 2: if they select "2-Yes I understand" of the arm 2 condition
#for arm 3: if they put anything in alias before going to link for video
#for arm 4: if they verify their belief in the statement they typed
df <- mutate(dfRCT, arm = ifelse(grepl("I hereby agree to and sign this statement", dfRCT$Q3), "1", ifelse(grepl("2", dfRCT$Q5), "2", ifelse(grepl("[[:alpha:]]", dfRCT$Q7), "3", ifelse(grepl("I confirm that the statement above reflects my reasons for requesting a vaccine exemption", dfRCT$Q12), "4", "" )))))

#check for duplicates
sum(duplicated(df$workerId))
                     
#see how many in each group
#length(which(df$arm == 1))
#length(which(df$arm == 2))
#length(which(df$arm == 3))
#length(which(df$arm == 4))

#see how many answered last question
#sum((length(which(df$Q24.1 == 0)) + (length(which(df$Q24.1 == 2)))))

#recode PACV variables
#10 (PACV 1).  a = 0 b = 2 c = 1
#11 (PACV 2).  a = 2 b = 0 c = 1
#12 (PACV 3).  a = 2 b = 0 c = 1
#13 (PACV 4).  a = 2 b = 0 c = 1
#14 (PACV 5).  a = 2 b = 0 c = 1
df$Q16 <- recode_factor(df$Q16, `1` = 0L, `2` = 2L, `3` = 1L)
df$Q18 <- recode_factor(df$Q18, `1` = 2L, `2` = 0L, `3` = 1L)
df$Q20 <- recode_factor(df$Q20, `1` = 2L, `2` = 0L, `3` = 1L)
df$Q22 <- recode_factor(df$Q22, `1` = 2L, `2` = 0L, `3` = 1L)
df$Q24 <- recode_factor(df$Q24, `1` = 2L, `2` = 0L, `3` = 1L)

df$PACV_sum
#make post TPACV scores numeric
df$Q16 <- as.numeric(as.character(df$Q16))
df$Q18 <- as.numeric(as.character(df$Q18))
df$Q20 <- as.numeric(as.character(df$Q20))
df$Q22 <- as.numeric(as.character(df$Q22))
df$Q24 <- as.numeric(as.character(df$Q24))

#sum post PACV scores
df$TPACV_Sum <- df$Q16 + df$Q18 + df$Q20 + df$Q22 + df$Q24

#descriptives

#regression of PACV scores after intervention
regTPACV <- lm(TPACV_Sum ~ arm + PACV_sum, data = df)
summary(regTPACV)

#standardize variables
scaleVACSCHL <- scale(df$VACSCHL)
scaleVACSCHLpost <- scale(df$Q26_1)
scaleNONMEDX <- scale(df$NONMEDX)
scaleNONMEDXpost <- scale(df$Q28_1)
scaleMEDX <- scale(df$MEDX)
scaleMEDXpost <- scale(df$Q30_1.1)

#regression of vaccine schedule
df$Q26_1 <- as.numeric(as.character(df$Q26_1))
regVACSCHED <- lm(Q26_1 ~ arm + VACSCHL, data = df)
summary(regVACSCHED)

#regression of standardized vaccine shedule
regstVACSCHL <- lm(scaleVACSCHLpost ~ arm + scaleVACSCHL, data = df)
summary(regstVACSCHL)

#regression of nonmedical exemption
df$Q28_1 <- as.numeric(as.character(df$Q28_1))
regNONMEDX <- lm(Q28_1 ~ arm + NONMEDX, data = df)
summary(regNONMEDX)

#regression of standardized nonmedical exemption
regstNONMEDX <- lm(scaleNONMEDXpost ~ arm + scaleNONMEDX, data = df)
summary(regstNONMEDX)

#regression of medical exemption
df$Q30_1.1 <- as.numeric(as.character(df$Q30_1.1))
regMEDX <- lm(Q30_1.1 ~ arm + MEDX, data = df)
summary(regMEDX)

#regression of standardized medical exemption
regstMEDX <- lm(scaleMEDXpost ~ arm + scaleMEDX, data = df)
summary(regstMEDX)

#run ANCOVA
#Duplicate for three intention

##write as CSV

write.csv(dfRCT, "dfRCT.csv")

