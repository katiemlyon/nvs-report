# Demographic characteristics of the sample
library(ggplot2)
library(likert)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}


## ---- age ----

agePlot <- ggplot(data=subset(nvs2018, !is.na(AGECAT)), aes(AGECAT)) + 
  geom_bar(aes(fill = factor(AGECAT))) + theme_classic() +
  ggtitle("Age Categories")

agePlot

## ---- gender ----

str(nvs2018$GENDER)
nvs2018$GENDER[nvs2018$GENDER=="9"] <- NA

gender = nvs2018$GENDER
gender[gender=="9"] <- NA

genderTable <- table(gender)
genderTable

propMale = round_df((genderTable)["0"]/sum(genderTable)*100,0)
propMale

propFemale = round_df((genderTable)["1"]/sum(genderTable)*100,0)
propFemale

maleAge <- subset(nvs2018, select = c(GENDER, AGE))
maleAge <- maleAge[which(maleAge$GENDER==0 & maleAge$AGE < 999),]
range(maleAge$AGE)
mean(maleAge$AGE)
maleAveAge <- round_df(mean(maleAge$AGE), 0)
maleAveAge

femAge <- subset(nvs2018, select = c(GENDER, AGE))
femAge <- femAge[which(femAge$GENDER==1 & femAge$AGE < 999),]
range(femAge$AGE)
mean(femAge$AGE)
femAveAge <- round_df(mean(femAge$AGE), 0)
femAveAge
##################

#gender <- subset(nvs2018, select = c(GENDER))
names(gender) = c(
  "Male",
  "Female")

genderlevels <- c('Male', 'Female')

# recode each factor and explicitly set the levels
for(i in seq_along(gender)) {
  gender[,i] <- factor(gender[,i], levels = genderlevels)
}

genderProp <- likert(gender)
genderProp
#plot(genderProp, centered=FALSE) + ggtitle(smTitle)

genderTable <- summary(genderProp)
str(genderTable)

genderTable$high <- round_df(genderTable$high, 0)

genderTable[with(genderTable, order(-high)), ] %>% select (Item, high)



genderPlot <- ggplot(data=subset(gender, !is.na(GENDER)), aes(GENDER)) + 
  geom_bar(aes(fill = factor(GENDER))) +
  ggtitle("Gender")
genderPlot

#################


# race - single variable
ETH <- subset(nvs2018, select = c(WHITE:OTHERETH))
ETH[ETH == "9"] <- NA

ETH$newvar <- rowSums(eth == "Yes")
ETH$Race <- NA
ETH$Race[ETH$newvar > 1] <- "Two or more races"
ETH$Race[ETH$newvar == 1 & ETH$WHITE == "Yes"] <- "White"
ETH$Race[ETH$newvar == 1 & ETH$HISPANIC == "Yes"] <- "Hispanic/Latino"
ETH$Race[ETH$newvar == 1 & ETH$AFRAMER == "Yes"] <- "African American/Black"
ETH$Race[ETH$newvar == 1 & ETH$ASIAN == "Yes"] <- "Asian"
ETH$Race[ETH$newvar == 1 & ETH$MIDEAST == "Yes"] <- "Middle Eastern"
ETH$Race[ETH$newvar == 1 & ETH$PACISL == "Yes"] <- "Pacific Islander"
ETH$Race[ETH$newvar == 1 & ETH$OTHERETH == "Yes"] <- "Other"
table(ETH$Race)

raceProp <- prop.table(table(ETH$Race))*100 # cell percentages
raceProp <- round_df(raceProp) # cell percentages
raceProp <- as.data.frame(raceProp)
raceProp <- raceProp[order(raceProp$Freq, decreasing = TRUE),]
colnames(raceProp) <- c("Race", "Proportion")
raceProp

raceProp <- raceProp %>% 
  mutate(Eth = "Race")

library(tidyverse)
library(ggthemes)

ggplot(raceProp, aes(x = ETH, y = Proportion, fill = Race)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  theme_economist(base_size = 1) +
  scale_fill_economist() +
#  theme_minimal(base_size = 16) +
#  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "right", 
        legend.title = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  ylab("Percentage") +
  xlab("Race")

ggplot(ETH, aes(x = ETH, y = Proportion, fill = Race)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)
###############

## ---- demographics ----

# Demographics
demographics <-
  subset(nvs2018,
         select = c(AGE, AGECAT, GENDER, SCHOOL, EDUCATION, WHITE:OTHERETH, INCOME, EMPLOYFT:EMPLOYOTH))
str(demographics)

library(dplyr)
agg <- count(demographics)
head(agg)
library(forcats)
ggplot(mutate(agg, AGECAT = fct_infreq(AGECAT))) + geom_bar(aes(x = AGECAT))
ggplot(mutate(agg, GENDER = fct_infreq(GENDER))) + geom_bar(aes(x = GENDER))
ggplot(mutate(agg, GENDER = fct_infreq(INCOME))) + geom_bar(aes(x = INCOME))
ggplot(mutate(agg, GENDER = fct_infreq(EDUCATION))) + geom_bar(aes(x = EDUCATION))

grid.arrange(ggplot(demographics)+geom_col(aes(gender, Freq)),
             ggplot(demographics)+geom_col(aes(Race, Freq)),
             ggplot(demographics)+geom_col(aes(AGECAT, Freq)),
             nrow = 1)
