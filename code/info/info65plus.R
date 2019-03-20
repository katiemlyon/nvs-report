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

nvs65plus <- nvs2018[which(nvs2018$AGECAT == "65+ years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems65 <- subset(nvs65plus, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems65[infoItems65=="0"] <- NA
infoItems65[infoItems65=="9"] <- NA
infoItems65 <- data.frame(apply(infoItems65, 2, as.factor))
str(infoItems65)
head(infoItems65)
names(infoItems65) = c(
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
  # This will throw an error because all the infoItems65 must have the same number of levels.
  lbad <- likert(infoItems65)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(infoItems65, class) #Verify that all the columns are indeed factors
sapply(infoItems65, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems65)) {
  infoItems65[,i] <- factor(infoItems65[,i], levels = infolevels)
}

infoSum65 <- likert(infoItems65)
infoSum65
summary(infoSum65)

# create table of information sources from summary statistics
infoTable65 <- summary(infoSum65)
str(infoTable65)

# round to whole number
infoTable65$high <- round_df(infoTable65$high, 0)

# sort information sources by most helpful
infoTable65[with(infoTable65, order(-high)), ] %>% dplyr::select (Item, high)


plot(infoSum65, centered=FALSE) + ggtitle(infoTitle)

## Other Website
otherWeb <- nvs65plus$OTHERWEBTEXT
otherWeb <- otherWeb[!is.na(nvs65plus$OTHERWEBTEXT)]
otherWeb

## Other Source
otherSource <- nvs65plus$OTHERSOURCETEXT
otherSource <- otherSource[!is.na(nvs65plus$OTHERSOURCETEXT)]
otherSource
