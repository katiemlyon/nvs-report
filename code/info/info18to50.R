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

nvs18to49 <- nvs2018[which(nvs2018$AGECAT == "18-34 years" | nvs2018$AGECAT == "35-49 years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems18 <- subset(nvs18to49, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems18[infoItems18=="0"] <- NA
infoItems18[infoItems18=="9"] <- NA
infoItems18 <- data.frame(apply(infoItems18, 2, as.factor))
str(infoItems18)
head(infoItems18)
names(infoItems18) = c(
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
  # This will throw an error because all the infoItems18 must have the same number of levels.
  lbad <- likert(infoItems18)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(infoItems18, class) #Verify that all the columns are indeed factors
sapply(infoItems18, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems18)) {
  infoItems18[,i] <- factor(infoItems18[,i], levels = infolevels)
}

infoSum18 <- likert(infoItems18)
infoSum18
summary(infoSum18)

# create table of information sources from summary statistics
infoTable18 <- summary(infoSum18)
str(infoTable18)

# round to whole number
infoTable18$high <- round_df(infoTable18$high, 0)

# sort information sources by most helpful
infoTable18[with(infoTable18, order(-high)), ] %>% dplyr::select (Item, high)


plot(infoSum18, centered=FALSE) + ggtitle(infoTitle)
