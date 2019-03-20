# EDUCATION
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

# function to round percentages to whole number
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# round percent
educ <- round_df(educ, 0) 
educ

educLevel <- NA
educLevel[educ < 12] <- "Less than HS"
educLevel[educ >= 12 & educ <= 15] <- "High School/Some College"
educLevel[educ >= 16 & educ <= 17] <- "College"
educLevel[educ >= 18] <- "Graduate School"
educLevel

#edlevels <- c('High School', 'College')

# recode each factor and explicitly set the levels
for(i in seq_along(education)) {
  education[,i] <- factor(education[,i])
}
edProp <- likert(education)
edProp
edTitle <- "Education"

edTable <- summary(edProp)
str(edTable)

edPlot <- ggplot(data=subset(education, !is.na(SCHOOL)), aes(SCHOOL)) + 
  geom_bar(aes) +
  ggtitle("Education")
edPlot




# create educ categories
attach(nvs2018)
nvs2018$EDCAT[SCHOOL < 12] <- "Less than HS"
nvs2018$EDCAT[SCHOOL >= 12 & SCHOOL <= 15] <- "High School/Some College"
nvs2018$EDCAT[SCHOOL >= 16 & SCHOOL <= 17] <- "College"
nvs2018$EDCAT[SCHOOL >= 18] <- "Graduate School"
detach(nvs2018)
table(nvs2018$EDCAT)
