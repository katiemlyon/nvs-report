# INCOME
# Categorical Variable

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

####################

incomeL <- nvs2018$INCOME
incomeL <- as.data.frame(incomeL)
table(incomeL)

# recode each factor and explicitly set the levels
for (i in seq_along(incomeL)) {
  incomeL[, i] <- factor(incomeL[, i])
}
incomeProp <- likert(incomeL)
incomeProp
incomeTitle <- "income"

incomeTable <- summary(incomeProp)
str(incomeTable)

incomePlot <-
  ggplot(data = subset(incomeL,!is.na(incomeL)), aes(incomeL)) +
  geom_bar(aes(fill = factor(incomeL))) +
  ggtitle("Income")
incomePlot



#######
names(incomeL) = c(
  "Less than $10,000",
  "$10,000-24,999",
  "$25,000-$34,999",
  "$35,000-$49,999",
  "$50,000-$74,999",
  "$75,000-$99,999",
  "$100,000-$149,999",
  "$150,000-$199,999",
  "$200,000 or more"
)