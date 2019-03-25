## @knitr satisfaction
require(likert)
nvs2018 <- read.csv("data/nvs2018.csv")

##### Overall Satisfaction
refSatTitle <- "How satisfied are you with the following?"

refSatItems <- subset(nvs2018, select = c(CONSERVSAT, QUALSAT))
refSatItems[refSatItems=="0"] <- NA
refSatItems[refSatItems=="9"] <- NA
#refSatItems <- mutate_if(refSatItems, is.numeric, as.factor)
refSatItems <- data.frame(apply(refSatItems, 2, as.factor))
str(refSatItems)

satlevels <- c('Not at all satisfied',
               'Slightly satisfied',
               'Moderately satisfied',
               'Very satisfied',
               'Extremely satisfied')

tryCatch({
  # This will throw an error because all the refSatItems must have the same number of levels.
  lbad <- likert(refSatItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})

sapply(refSatItems, class) #Verify that all the columns are indeed factors
sapply(refSatItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(refSatItems)) {
  refSatItems[,i] <- factor(refSatItems[,i], levels = satlevels)
}

refSat <- likert(refSatItems)
refSat
summary(refSat)
plot(refSat)

# create table of information sources from summary statistics
refSatTable <- summary(refSat)
str(refSatTable)

# function to round percentages to whole number
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

# round percent satisfied
refSatTable$high <- round_df(refSatTable$high, 0)

# sort percent satisfied
refSatTable[with(refSatTable, order(-high)), ] %>% dplyr::select (Item, high)

qual <- subset(refSatTable, Item == "QUALSAT")
qualSat = qual$high

conserv <- subset(refSatTable, Item == "CONSERVSAT")
conservSat = conserv$high
