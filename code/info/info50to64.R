require(dplyr)
require(likert)

# function to round percentages to whole number
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

nvs50to64 <- nvs2018[which(nvs2018$AGECAT == "50-64 years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems50 <- subset(nvs50to64, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems50[infoItems50=="0"] <- NA
infoItems50[infoItems50=="9"] <- NA
infoItems50 <- data.frame(apply(infoItems50, 2, as.factor))
str(infoItems50)
head(infoItems50)
names(infoItems50) = c(
  "Personal knowledge from previous visits",
  "Word of mouth", 
  "People in the local community",
  "Refuge employees or volunteers",
  "Printed map or atlas",
  "Web-based map",
  "Refuge website",
  "Travel website",
  "Other website",
  "Social media",
  "Recreation club or organization",
  "Refuge printed information",
  "Kiosks/displays/exhibits at this refuge",
  "Travel guidebook or other book",
  "Tourist information or welcome center",
  "Other source"
)

infolevels <- c('Not at all helpful',
                'Slightly helpful',
                'Moderately helpful',
                'Very helpful',
                'Extremely helpful')

tryCatch({
  # This will throw an error because all the infoItems50 must have the same number of levels.
  lbad <- likert(infoItems50)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(infoItems50, class) #Verify that all the columns are indeed factors
sapply(infoItems50, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems50)) {
  infoItems50[,i] <- factor(infoItems50[,i], levels = infolevels)
}

infoSum50 <- likert(infoItems50)
infoSum50
summary(infoSum50)

# create table of information sources from summary statistics
infoTable50 <- summary(infoSum50)
str(infoTable50)

# round to whole number
infoTable50$high <- round_df(infoTable50$high, 0)

# sort information sources by most helpful
infoTable50[with(infoTable50, order(-high)), ] %>% dplyr::select (Item, high)


plot(infoSum50, centered=FALSE) + ggtitle(infoTitle)

## Other Website
otherWeb <- nvs50to64$OTHERWEBTEXT
otherWeb <- otherWeb[!is.na(nvs50to64$OTHERWEBTEXT)]
otherWeb

## Other Source
otherSource <- nvs50to64$OTHERSOURCETEXT
otherSource <- otherSource[!is.na(nvs50to64$OTHERSOURCETEXT)]
otherSource
