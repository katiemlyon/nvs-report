## @knitr demographics

nvs2018 <- read.csv("data/nvs2018.csv")

library(dplyr)
library(likert)
library(XML)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(plyr)

source("code/functions/round_df.R")

###########################################
# Demographic characteristics of the sample
###########################################

demographics <-
  subset(nvs2018,
         select = c(AGE, AGECAT, GENDER, SCHOOL, EDUCATION, WHITE:OTHERETH, INCOME, EMPLOYFT:EMPLOYOTH))
str(demographics)

library(forcats)
ggplot(mutate(demographics, AGECAT = fct_infreq(AGECAT))) + geom_bar(aes(x = AGECAT))
ggplot(mutate(demographics, GENDER = fct_infreq(GENDER))) + geom_bar(aes(x = GENDER))
ggplot(mutate(demographics, GENDER = fct_infreq(INCOME))) + geom_bar(aes(x = INCOME))
ggplot(mutate(demographics, GENDER = fct_infreq(EDUCATION))) + geom_bar(aes(x = EDUCATION))

# Work on creating a table with multiple plots
#grid.arrange(ggplot(demographics)+geom_col(aes(GENDER, Freq)),
#             ggplot(demographics)+geom_col(aes(Race, Freq)),
#             ggplot(demographics)+geom_col(aes(AGECAT, Freq)),
#             nrow = 1)

######################################
# age
######################################

table(nvs2018$AGECAT)
age <- subset(nvs2018,!is.na(AGECAT))

agePlot <- ggplot(data = age, aes(AGECAT)) +
  geom_bar(aes(fill = factor(AGECAT))) +
  theme_classic() +
  ggtitle("Age Categories")
agePlot

age <- na.omit(nvs2018$AGE)
quantile(age)

agePlot <- ggplot(data=subset(nvs2018, !is.na(AGECAT)), aes(AGECAT)) +
  geom_bar(aes(fill = factor(AGECAT))) +
  theme_classic() +
  ggtitle("Age Categories")

agePlot

######################################
# gender
######################################

table(nvs2018$GENDER)
nvs2018$GENDER <-
  droplevels(nvs2018$GENDER, exclude = c("1951", "1959")) #OSU data cleanup

gender <- nvs2018$GENDER
str(gender)
levels(gender)

genderTable <- table(gender)
genderTable

propMale = round_df((genderTable)["Male"] / sum(genderTable) * 100, 0)
propMale

propFemale = round_df((genderTable)["Female"] / sum(genderTable) * 100, 0)
propFemale

maleAge <- subset(nvs2018, select = c(GENDER, AGE))
maleAge <-
  maleAge[which(maleAge$GENDER == "Male" & maleAge$AGE < 999), ]
range(maleAge$AGE)
mean(maleAge$AGE)
maleAveAge <- round_df(mean(maleAge$AGE), 0)
maleAveAge

femAge <- subset(nvs2018, select = c(GENDER, AGE))
femAge <- femAge[which(femAge$GENDER == "Female" &
                         femAge$AGE < 999), ]
range(femAge$AGE)
mean(femAge$AGE)
femAveAge <- round_df(mean(femAge$AGE), 0)
femAveAge

######################################
# education
######################################

str(nvs2018$SCHOOL)
range(nvs2018$SCHOOL, na.rm = TRUE)
nvs2018$SCHOOL[nvs2018$SCHOOL == "163"] <- NA #OSU data cleaning
table(nvs2018$SCHOOL)

#education <- subset(nvs2018, select = c(SCHOOL))
education <- nvs2018$SCHOOL
education <- education[!is.na(education)]

#calculate mean
educ <- mean(education)
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

######################################
# income
######################################

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



#################

# race - single variable

ethnicity <- subset(nvs2018, select = c(WHITE:OTHERETH))

######################################
# income
######################################

# create a data frame
pyramidDF <- subset(nvs2018,
                    select = c(AGE, GENDER))
pyramidDF <- na.omit(pyramidDF)
str(pyramidDF)

# convert Age column to a factor with the required break-points:
AgesFactor <- ordered( cut(pyramidDF$AGE, breaks = c(18,seq(30,100,10)),
                           include.lowest = TRUE))

levels(AgesFactor)

pyramidDF$AGE <- AgesFactor

## Plotting
gg <- ggplot(data = pyramidDF, aes(x=AGE))

gg.male <- gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Male',],
           aes( y = ..count../sum(..count..), fill = AGE)) +
  scale_y_continuous('', labels = scales::percent) +
  theme(legend.position = 'none'
        ,axis.text.y = theme_bw()$axis.text.y
        ,axis.ticks.y = element_blank()
        ,axis.title.y = element_blank()
        ,plot.title = element_text(size = 11.5)
        ,plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm")
        ,plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  ggtitle("Male") +
  coord_flip()

gg.female <-  gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Female',],
           aes( y = ..count../sum(..count..), fill = AGE)) +
  scale_y_continuous('', labels = scales::percent,
                     trans = 'reverse') +
  theme(legend.position = 'none'
        ,axis.text.y = element_blank()
        ,axis.ticks.y = element_blank()
        ,plot.title = element_text(size = 11.5)
        ,plot.margin=unit(c(0.1,0,0.1,0.05),"cm")
        ,plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  ggtitle("Female") +
  coord_flip() +
  ylab("Age")


## Plutting it together
agePyramid <- grid.arrange(gg.female,
                           gg.male,
                           widths=c(0.4,0.6),
                           ncol=2,
                           top = textGrob("Age Distribution of Visitors to this Refuge",
                                          gp=gpar(fontsize=20))
)

agePyramid
