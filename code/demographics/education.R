library(ggplot2)

#load functions
source("code/functions/calc_pct.R")
source("code/functions/round_df.R")

#read data
nvs2018 <- read.csv("data/nvs2018.csv")

###########################################
# Education
###########################################

str(nvs2018$SCHOOL)
range(nvs2018$SCHOOL, na.rm=TRUE)
table(nvs2018$SCHOOL)

#education <- subset(nvs2018, select = c(SCHOOL))
education <- nvs2018$SCHOOL
education <- education[!is.na(education)]

# calculate mean education level
educ = mean(education)
educ

# round percent to whole number
educ <- round_df(educ, 0)
educ

# specify education levels for output
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

###########
education <- nvs2018$SCHOOL
edPlot <- ggplot(data=subset(education, !is.na(SCHOOL)), aes(SCHOOL)) +
  geom_bar(aes) +
  ggtitle("Education")
edPlot


# What are the education levels?
# EDCAT is a factor (i.e., categorical) variable, a bar chart
# is a great visualization to use.
#
ggplot(titanic, aes(x = Survived)) +
  geom_bar()

# If you really want percentages.
prop.table(table(titanic$Survived))

# Add some customization for labels and theme.
ggplot(titanic, aes(x = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates")


###########
# What are the education levels?
# EDCAT is a factor (i.e., categorical) variable, a bar chart
# is a great visualization to use.

education <- subset(nvs2018, select = EDUCATION)
education <- na.omit(education)

ggplot(education, aes(x = EDUCATION)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Education",
       y = "Percent",
       title = "Education") +
  coord_flip()

## simpler bar chart
ggplot(education) +
  stat_count(mapping = aes(x=EDUCATION, y=..prop.., group=1)) +
  labs(x = "Education",
       y = "Percent",
       title = "Education") +
  coord_flip() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank())
