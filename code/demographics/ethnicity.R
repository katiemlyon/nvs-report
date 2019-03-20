# ETHNICITY
# Multiple Dichotomous Variables
str(nvs2018$WHITE)

ethnicity <- subset(nvs2018, select = c(WHITE:OTHERETH))
ethnicity[ethnicity == "9"] <- NA

names(ethnicity) = c(
  "White",
  "Hispanic, Latino, or Spanish",
  "Black or African American",
  "Asian",
  "American Indian or Alaska Native",
  "Middle Eastern or North African",
  "Native Hawaiian or Other Pacific Islander",
  "Some other race or ethnicity"
)


dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for (i in seq_along(ethnicity)) {
  ethnicity[, i] <- factor(ethnicity[,i], levels = dichotlevels)
}
ethProp <- likert(ethnicity)
ethProp
ethTitle <- "Ethnicity"
#plot(ethProp, centered=FALSE) + ggtitle(ethTitle)

ethTable <- summary(ethProp)
str(ethTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

ethTable$high <- round_df(ethTable$high, 0)

ethTable[with(ethTable, order(-high)),] %>% select (Item, high)


###################
# race - single variable
eth <- subset(nvs2018, select = c(WHITE:OTHERETH))
eth[eth == "9"] <- NA

eth$newvar <- rowSums(eth == "Yes")
eth$Race <- NA
eth$Race[eth$newvar > 1] <- "Two or more races"
eth$Race[eth$newvar == 1 & eth$WHITE == "Yes"] <- "White"
eth$Race[eth$newvar == 1 & eth$HISPANIC == "Yes"] <- "Hispanic/Latino"
eth$Race[eth$newvar == 1 & eth$AFRAMER == "Yes"] <- "African American/Black"
eth$Race[eth$newvar == 1 & eth$ASIAN == "Yes"] <- "Asian"
eth$Race[eth$newvar == 1 & eth$MIDEAST == "Yes"] <- "Middle Eastern"
eth$Race[eth$newvar == 1 & eth$PACISL == "Yes"] <- "Pacific Islander"
eth$Race[eth$newvar == 1 & eth$OTHERETH == "Yes"] <- "Other"
table(eth$Race)

race = eth$Race