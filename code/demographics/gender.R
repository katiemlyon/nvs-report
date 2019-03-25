# GENDER
# Single Dichotomous Variables

nvs2018 <- read.csv("data/nvs2018.csv")

library(likert)
source("code/functions/round_df.R")

######################################
# gender
######################################

str(nvs2018$GENDER)
#nvs2018$GENDER[nvs2018$GENDER=="9"] <- NA

gender <- na.omit(nvs2018$GENDER)
str(gender)

genderTable <- table(gender)
genderTable

propMale = round_df((genderTable)["Male"]/sum(genderTable)*100,0)
propMale #percent of male visitors

propFemale = round_df((genderTable)["Female"]/sum(genderTable)*100,0)
propFemale #percent of female visitors

maleAge <- subset(nvs2018, select = c(GENDER, AGE))
maleAge <- maleAge[which(maleAge$GENDER=="Male" & maleAge$AGE < 999),]
range(maleAge$AGE)
mean(maleAge$AGE)
maleAveAge <- round_df(mean(maleAge$AGE), 0) #round to whole number
maleAveAge #average age of male visitors

femAge <- subset(nvs2018, select = c(GENDER, AGE))
femAge <- femAge[which(femAge$GENDER=="Female" & femAge$AGE < 999),]
range(femAge$AGE)
mean(femAge$AGE)
femAveAge <- round_df(mean(femAge$AGE), 0) #round to whole number
femAveAge #average age of female visitors

##################

genderDF <- as.data.frame(gender)
str(genderDF)

genderProp <- likert(genderDF)
genderProp
#plot(genderProp, centered=FALSE) + ggtitle(smTitle)

genderBarGraph <- ggplot(data=subset(genderDF, !is.na(gender)), aes(gender)) +
  geom_bar(aes(fill = factor(gender))) +
  theme_classic() +
  ggtitle("Gender")
genderBarGraph

