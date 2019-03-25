## @knitr transMode

nvs2018 <- read.csv("data/nvs2018.csv")
library(dplyr)
#library(tidyr)
library(likert)
#library(waffle)
#library(extrafont)
#library(ggplot2)

#######################
# Transportation Mode
#PRIVEH_LA, PRIVEHTR_LA, RVUSE_LA, REFBUS_LA, TOURBUS_LA, PUBTRANS_LA, MOTOCYC_LA, BIKEUSE_LA, WALKHIKE_LA, BOATUSE_LA
#PRIVEH_REF, PRIVEHTR_REF, RVUSE_REF, REFBUS_REF, TOURBUS_REF, PUBTRANS_REF, MOTOCYC_REF, BIKEUSE_REF, WALKHIKE_REF, BOATUSE_REF

##### Trans Mode Local
transModeLocal <-
  subset(
    nvs2018,
    select = c(
      PRIVEH_LA,
      PRIVEHTR_LA,
      RVUSE_LA,
      REFBUS_LA,
      TOURBUS_LA,
      PUBTRANS_LA,
      MOTOCYC_LA,
      BIKEUSE_LA,
      WALKHIKE_LA,
      BOATUSE_LA
    )
  )
str(transModeLocal)
#transModeLocal$BOATUSE_LA <- droplevels(transModeLocal$BOATUSE_LA, exclude = c("2")) #OSU data cleanup
#transModeLocal[transModeLocal == "0"] <- NA
transModeLocal[transModeLocal == "2"] <- NA #OSU data cleanup
transModeLocal[transModeLocal == "9"] <- NA
#transModeLocal <- data.frame(apply(transModeLocal, 2, as.factor))
str(transModeLocal)

##### Trans Mode within Refuge
transModeRefuge <-
  subset(
    nvs2018,
    select = c(
      PRIVEH_REF,
      PRIVEHTR_REF,
      RVUSE_REF,
      REFBUS_REF,
      TOURBUS_REF,
      PUBTRANS_REF,
      MOTOCYC_REF,
      BIKEUSE_REF,
      WALKHIKE_REF,
      BOATUSE_REF
    )
  )
transModeRefuge[transModeRefuge == "9"] <- NA
str(transModeRefuge)

names(transModeLocal) = c(
  "Private vehicle without trailer",
  "Private vehicle with trailer",
  "RV",
  "Refuge tram",
  "Tour bus",
  "Public transportation",
  "Motorcycle",
  "Bicycle",
  "Foot",
  "Boat"
)

dichotlevels <- c('No', 'Yes')

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(transModeLocal)) {
  transModeLocal[,i] <- factor(transModeLocal[,i], levels = dichotlevels)
}
transModeLocalProp <- likert(transModeLocal)
transModeLocalProp

transModeLocalTitle <- "Transportation Mode - Local Area"

transModeLocalTable <- summary(transModeLocalProp)
str(transModeLocalTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

transModeLocalTable$high <- round_df(transModeLocalTable$high, 0)

transModeLocalTable[with(transModeLocalTable, order(-high)), ] %>% select (Item, high)

transLocal1 <- transModeLocalTable$Item [1]
transLocal1

transLocal2 <- transModeLocalTable$Item [2]
transLocal2

transLocal3 <- transModeLocalTable$Item [3]
transLocal3

transLocalProp1 <- transModeLocalTable$high [1]
transLocalProp1

transLocalProp2 <- transModeLocalTable$high [2]
transLocalProp2

transLocalProp3 <- transModeLocalTable$high [3]
transLocalProp3

# Histogram
#ggplot(transModeLocalTable, aes(x=high)) + geom_histogram() + theme_classic()


##### Trans Mode within Refuge

#transModeRefuge[transModeRefuge == "0"] <- NA
transModeRefuge[transModeRefuge == "9"] <- NA
transModeRefuge <- data.frame(apply(transModeRefuge, 2, as.factor))
str(transModeRefuge)

names(transModeRefuge) = c(
  "Private vehicle without trailer",
  "Private vehicle with trailer",
  "RV",
  "Refuge tram",
  "Tour bus",
  "Public transportation",
  "Motorcycle",
  "Bicycle",
  "Foot",
  "Boat"
)

dichotlevels <- c('No', 'Yes')

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(transModeRefuge)) {
  transModeRefuge[,i] <- factor(transModeRefuge[,i], levels = dichotlevels)
}
transModeRefugeProp <- likert(transModeRefuge)
transModeRefugeProp
transModeRefugeTitle <- "Transportation Mode - On the Refuge"

transModeRefTable <- summary(transModeRefugeProp)
str(transModeRefTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

transModeRefTable$high <- round_df(transModeRefTable$high, 0)

transModeRefTable[with(transModeRefTable, order(-high)), ] %>% select (Item, high)

transRef1 <- transModeRefTable$Item [1]
transRef1

transRef2 <- transModeRefTable$Item [2]
transRef2

transRef3 <- transModeRefTable$Item [3]
transRef3

transRefProp1 <- transModeRefTable$high [1]
transRefProp1

transRefProp2 <- transModeRefTable$high [2]
transRefProp2

transRefProp3 <- transModeRefTable$high [3]
transRefProp3

# Histogram

#ggplot(transModeRefTable, aes(x=high)) + geom_histogram() + theme_classic()

TransModeTable <- merge(transModeLocalTable, transModeRefTable, by = "Item")
TransModeTable


# Trans Mode
#transMode <- data.frame(transModeLocal, transModeRefuge)
#str(transMode)
#library(reshape2)
#df <- melt(transModeLocal, id.vars=1:10)

transModeLocalT <- t(transModeLocal)
str(transModeLocal)

transModeRefugeT <- t(transModeRefuge)
str(transModeRefuge)

transMode <- cbind(transModeLocal, transModeRefuge)
#transMode <- as.data.frame(transMode)
str(transMode)

# Ordered bar chart
library(ggplot2)
theme_set(theme_minimal())

