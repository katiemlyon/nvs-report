#### GENDER
library(likert)

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

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

genderTable$high <- round_df(genderTable$high, 0)

genderTable[with(genderTable, order(-high)), ] %>% select (Item, high)



genderPlot <- ggplot(data=subset(gender, !is.na(GENDER)), aes(GENDER)) + 
  geom_bar(aes(fill = factor(GENDER))) +
  ggtitle("Gender")
genderPlot

