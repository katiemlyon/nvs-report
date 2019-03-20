## ---- visit ----
  
nvs2018 <- read.csv("data/nvs2018.csv")

#library(plyr)
library(dplyr)
library(ggplot2)
library(likert)
library(tidyverse)
#library(waffle)

# function to round percentages to whole number
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

#################
# age

table(nvs2018$AGECAT)
age <- subset(nvs2018, !is.na(AGECAT))

agePlot <- ggplot(data=age, aes(AGECAT)) + 
  geom_bar(aes(fill = factor(AGECAT))) + theme_classic() +
  ggtitle("Age Categories")
agePlot

#################
# gender

table(nvs2018$GENDER)
nvs2018$GENDER <- droplevels(nvs2018$GENDER, exclude = c("1951", "1959")) #OSU data cleanup

gender <- nvs2018$GENDER
str(gender)
levels(gender)

genderTable <- table(gender)
genderTable

propMale = round_df((genderTable)["Male"]/sum(genderTable)*100,0)
propMale

propFemale = round_df((genderTable)["Female"]/sum(genderTable)*100,0)
propFemale

maleAge <- subset(nvs2018, select = c(GENDER, AGE))
maleAge <- maleAge[which(maleAge$GENDER=="Male" & maleAge$AGE < 999),]
range(maleAge$AGE)
mean(maleAge$AGE)
maleAveAge <- round_df(mean(maleAge$AGE), 0)
maleAveAge

femAge <- subset(nvs2018, select = c(GENDER, AGE))
femAge <- femAge[which(femAge$GENDER=="Female" & femAge$AGE < 999),]
range(femAge$AGE)
mean(femAge$AGE)
femAveAge <- round_df(mean(femAge$AGE), 0)
femAveAge

###################
# education

str(nvs2018$SCHOOL)
range(nvs2018$SCHOOL, na.rm=TRUE)
nvs2018$SCHOOL[nvs2018$SCHOOL=="163"] <- NA #OSU data cleaning
table(nvs2018$SCHOOL)

#education <- subset(nvs2018, select = c(SCHOOL))
education <- nvs2018$SCHOOL
education <- education[!is.na(education)]

#calculate mean
educ = mean(education)
educ

# round percent
educ <- round_df(educ, 0) 
educ

educLevel <- NA
educLevel[educ < 12] <- "Less than HS"
educLevel[educ >= 12 & educ <= 15] <- "High School/Some College"
educLevel[educ >= 16 & educ <= 17] <- "College"
educLevel[educ >= 18] <- "Graduate School"
educLevel

###################
# income

str(nvs2018$INCOME)
#range(nvs2018$INCOME, na.rm = TRUE)
#nvs2018$INCOME[nvs2018$INCOME == "999"] <- NA

income <- nvs2018$INCOME
income <- as.numeric(income)
str(income)
#income[income=="999"] <- NA
income <- income[!is.na(income)]

#calculate median income
medIncome = median(income)
medIncome

incLevel <- NA
incLevel[medIncome == 1] <- "Less than $10,000"
incLevel[medIncome == 2] <- "$10,000-24,999"
incLevel[medIncome == 3] <- "$25,000-$34,999"
incLevel[medIncome == 4] <- "$35,000-$49,999"
incLevel[medIncome == 5] <- "$50,000-$74,999"
incLevel[medIncome == 6] <- "$75,000-$99,999"
incLevel[medIncome == 7] <- "$100,000-$149,999"
incLevel[medIncome == 8] <- "$150,000-$199,999"
incLevel[medIncome == 9] <- "$200,000 or more"
incLevel

###################
## activities

act12 <-
  subset(
    nvs2018,
    select = c(WILDOB:SPEVACT)
  )
str(act12)

names(act12) = c(
  "Wildlife observation",
  "Bird watching",
  "Photography",
  "Big game hunting",
  "Upland/Small game hunting",
  "Waterfowl/Migratory bird hunting",
  "Freshwater fishing",
  "Saltwater fishing",
  "Hiking/Walking",
  "Jogging/Running/Exercising",
  "Bicycling",
  "Auto tour route/Driving",
  "Motorized boating",
  "Nonmotorized boating",
  "Foraging",
  "Picnicking",
  "Volunteering",
  "Environmental education",
  "Interpretative program",
  "Refuge special event"
)

dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for(i in seq_along(act12)) {
  act12[,i] <- factor(act12[,i], levels = dichotlevels)
}
act12Prop <- likert(act12)
act12Prop
act12Title <- "Activities"

act12Table <- summary(act12Prop)
str(act12Table)

act12Table$high <- round_df(act12Table$high, 0)

act12Table[with(act12Table, order(-high)), ] %>% select (Item, high)

## Primary activity
# subset primary activity text
str(nvs2018$PRIMACTCODED)
nvs2018$PRIMACTCODED[nvs2018$PRIMACTCODED=="999"] <- NA

primact <- nvs2018$PRIMACTCODED
primact <- droplevels(primact, "999")
str(primact)

# Frequency Table 
primactFreq <- table(primact)
primactFreq <- as.data.frame(primactFreq)
primactFreq <- primactFreq[order(primactFreq$Freq, decreasing = TRUE),]
primactFreq # print table 

primactProp <- prop.table(table(primact))*100 # cell percentages
primactProp <- round_df(primactProp) # cell percentages
primactProp <- as.data.frame(primactProp)
primactProp <- primactProp[order(primactProp$Freq, decreasing = TRUE),]
primactProp

sum(primactProp$Freq)
#############

#nvs2018["TRIPPURP"] <- lapply(nvs2018["TRIPPURP"] , factor)
group <- subset(nvs2018, select = c(ADULTNUM:MINORNUM))
summary(group)


#############
## Visits to Public Lands

# This NWR
nvs2018$REFLASTYR[nvs2018$REFLASTYR=="999"] <- NA
str(nvs2018$REFLASTYR)
nvs2018$REFLASTYR <- as.numeric(nvs2018$REFLASTYR)
refLast12 <- na.omit(nvs2018$REFLASTYR)
mean(refLast12)
summary(refLast12)
refLength = length(refLast12[refLast12])
refOne = round_df(length(refLast12[refLast12 == 1])/refLength*100)
refTwo = round_df(length(refLast12[refLast12 == 2])/refLength*100)

# Other NWRs
nvs2018$NWRLASTYR[nvs2018$NWRLASTYR=="999"] <- NA
str(nvs2018$NWRLASTYR)
nvs2018$NWRLASTYR <- as.numeric(nvs2018$NWRLASTYR)
nwrLast12 <- na.omit(nvs2018$NWRLASTYR)
mean(nwrLast12)
summary(nwrLast12)
nwrLength = length(nwrLast12[nwrLast12])
nwrZero = round_df(length(nwrLast12[nwrLast12 == 0])/nwrLength*100)
nwrOne = round_df(length(nwrLast12[nwrLast12 == 1])/nwrLength*100)
nwrTwo = round_df(length(nwrLast12[nwrLast12 == 2])/nwrLength*100)

# Other Public Lands
nvs2018$OTHPUBLASTYR[nvs2018$OTHPUBLASTYR=="999"] <- NA
str(nvs2018$OTHPUBLASTYR)
nvs2018$OTHPUBLASTYR <- as.numeric(nvs2018$OTHPUBLASTYR)
othLast12 <- na.omit(nvs2018$OTHPUBLASTYR)
mean(othLast12)
summary(othLast12)
othLength = length(othLast12[othLast12])
othZero = round_df(length(othLast12[othLast12 == 0])/othLength*100)
othOne = round_df(length(othLast12[othLast12 == 1])/othLength*100)
othTwo = round_df(length(othLast12[othLast12 == 2])/othLength*100)

nvs2018 %>% 
  select(REFLASTYR:OTHPUBLASTYR) %>%
  na.omit -> visits
str(visits)
range(visits)

visits[visits > 10 & visits <= 20] <- "11-20"
visits[visits > 20 & visits <= 49] <- "21-49"
visits[visits > 50 & visits <= 99] <- "50-99"
visits[visits > 100 & visits <= 199] <- "100-199"
visits[visits > 200 & visits <= 299] <- "200-299"
visits[visits > 300 & visits <= 399] <- "300+"

names(visits) = c("This Refuge", "Other NWRs", "Other Public Lands")
str(visits)

visit_levels <- names(visits)[order(visits)]
str(visits)

###################
# Season

season <- subset(nvs2018, select =  c(SPRVIS, SUMVIS, FALLVIS, WINTVIS))
str(season)
season$SPRVIS <- as.factor(season$SPRVIS)
season$SUMVIS <- as.factor(season$SUMVIS)
season$FALLVIS <- as.factor(season$FALLVIS)
season$WINTVIS <- as.factor(season$WINTVIS)
seasonL <- likert(season)
seasonT <- summary(season)
str(seasonT)
seasonT


nvs2018 %>% 
  select(SPRVIS:WINTVIS) %>%
  na.omit -> season
str(season)

names(season) = c("Spring", "Summer", "Fall", "Winter")
str(season)

season_levels <- names(season)[order(season)]
season_levels  # notice the changed order of factor levels

season %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(answer),
         items = factor(items)) -> season2

season2$items <- factor(season2$items, levels = season_levels)

seasonBar <- ggplot(season2, aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  scale_x_discrete(limits = levels(season_levels)) + scale_fill_brewer(palette = "Greens")

seasonBar

################################

## Visitor Center
summary(nvs2018$VISCEN)

#nvs2018["VISCEN"] <- lapply(nvs2018["VISCEN"] , factor)
vcAct <- subset(nvs2018, select = c(ASKINFO:OTHERVIS))
summary(vcAct)

vcOther <- na.omit(nvs2018$OTHERVISTXT)
vcOther <- sort(vcOther)
vcOther
