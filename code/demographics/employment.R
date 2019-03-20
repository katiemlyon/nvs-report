# EMPLOYMENT
# Multiple Dichotomous Variables

str(nvs2018$EMPLOYFT)

employ <- subset(nvs2018, select = c(EMPLOYFT:EMPLOYOTH))
employ[employ == "9"] <- NA

names(employ) = c(
  "Employed full-time",
  "Employed part-time",
  "Self-employed",
  "Unemployed",
  "Homemaker/caregiver",
  "Student",
  "Retired",
  "Disabled/unable to work",
  "Other"
)


dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for (i in seq_along(employ)) {
  employ[, i] <- factor(employ[,i], levels = dichotlevels)
}
employProp <- likert(employ)
employProp
employTitle <- "employ"
#plot(employProp, centered=FALSE) + ggtitle(employTitle)

employTable <- summary(employProp)
str(employTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

employTable$high <- round_df(employTable$high, 0)

employTable[with(employTable, order(-high)), ] %>% select (Item, high)

