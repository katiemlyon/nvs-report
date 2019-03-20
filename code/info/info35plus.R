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

nvs35plus <- nvs2018[which(nvs2018$AGECAT == "35-49 years" | nvs2018$AGECAT == "50-64 years" | nvs2018$AGECAT == "65+ years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems35plus <- subset(nvs35plus, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems35plus[infoItems35plus=="0"] <- NA
infoItems35plus[infoItems35plus=="9"] <- NA
infoItems35plus <- data.frame(apply(infoItems35plus, 2, as.factor))
str(infoItems35plus)
head(infoItems35plus)
names(infoItems35plus) = c(
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
  # This will throw an error because all the infoItems35plus must have the same number of levels.
  lbad <- likert(infoItems35plus)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(infoItems35plus, class) #Verify that all the columns are indeed factors
sapply(infoItems35plus, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems35plus)) {
  infoItems35plus[,i] <- factor(infoItems35plus[,i], levels = infolevels)
}

infoSum35plus <- likert(infoItems35plus)
infoSum35plus
summary(infoSum35plus)

# create table of information sources from summary statistics
infoTable35plus <- summary(infoSum35plus)
str(infoTable35plus)

# round to whole number
infoTable35plus$high <- round_df(infoTable35plus$high, 0)

# sort information sources by most helpful
infoTable35plus[with(infoTable35plus, order(-high)), ] %>% dplyr::select (Item, high)


plot(infoSum35plus, centered=FALSE) + ggtitle(infoTitle)

## Other Website
otherWeb <- nvs35plus$OTHERWEBTEXT
otherWeb <- otherWeb[!is.na(nvs35plus$OTHERWEBTEXT)]
otherWeb

## Other Source
otherSource <- nvs35plus$OTHERSOURCETEXT
otherSource <- otherSource[!is.na(nvs35plus$OTHERSOURCETEXT)]
otherSource
