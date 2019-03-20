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

nvs35to49 <- nvs2018[which(nvs2018$AGECAT == "35-49 years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems35 <- subset(nvs35to49, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems35[infoItems35=="0"] <- NA
infoItems35[infoItems35=="9"] <- NA
infoItems35 <- data.frame(apply(infoItems35, 2, as.factor))
str(infoItems35)
head(infoItems35)
names(infoItems35) = c(
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
  # This will throw an error because all the infoItems35 must have the same number of levels.
  lbad <- likert(infoItems35)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(infoItems35, class) #Verify that all the columns are indeed factors
sapply(infoItems35, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems35)) {
  infoItems35[,i] <- factor(infoItems35[,i], levels = infolevels)
}

infoSum35 <- likert(infoItems35)
infoSum35
summary(infoSum35)

# create table of information sources from summary statistics
infoTable35 <- summary(infoSum35)
str(infoTable35)

# round to whole number
infoTable35$high <- round_df(infoTable35$high, 0)

# sort information sources by most helpful
infoTable35[with(infoTable35, order(-high)), ] %>% dplyr::select (Item, high)


plot(infoSum35, centered=FALSE) + ggtitle(infoTitle)

## Other Website
otherWeb <- nvs35to49$OTHERWEBTEXT
otherWeb <- otherWeb[!is.na(nvs35to49$OTHERWEBTEXT)]
otherWeb

## Other Source
otherSource <- nvs35to49$OTHERSOURCETEXT
otherSource <- otherSource[!is.na(nvs35to49$OTHERSOURCETEXT)]
otherSource
