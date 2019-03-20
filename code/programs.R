# Visitor Programs


## @knitr programs

nvs2018 <- readr::read_csv("data/nvs2018.csv")
library(dplyr)
library(likert)

# Subset Visitor Programs
programs <- subset(nvs2018, select = c(YOUTHPROG:NOPROG, AGECAT))
programs[programs=="9"] <- NA
programs <- programs[complete.cases(programs),]


progItems <- subset(programs, select = c(YOUTHPROG:NOPROG))
progItems[progItems=="9"] <- NA
progItems <- data.frame(apply(progItems, 2, as.factor))
#str(progItems)
head(progItems)
ncol(progItems)

names(progItems) = c(
  "Programs that engage youth",
  "Programs that focus on family/multiple-generations",
  "Programs that teach skills to visitors",
  "Programs that highlight unique local culture",
  "Programs that focus on creative pursuits",
  "Programs that support people with accessibility concerns",
  "Other (specify)",
  "I do not typically participate in refuge programs"
)

dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for(i in seq_along(progItems)) {
  progItems[,i] <- factor(progItems[,i], levels = dichotlevels)
}

str(progItems)
head(progItems)
ncol(progItems)

progProp <- likert(progItems)
progProp
programsTitle <- "Programs"

progPlot <- plot(progProp, centered=FALSE) + ggtitle(programsTitle)
progPlot

## @knitr progAge

#plot by age group
progAge <- likert(progItems, grouping=programs$AGECAT)
str(progAge$results)
progAgePlot <- plot(progAge) + ggtitle(programsTitle)
progAgePlot

##
progTable <- summary(progProp)
str(progTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

progTable$high <- round_df(progTable$high, 0)

progTable[with(progTable, order(-high)), ] %>% dplyr::select (Item, high)

## @knitr othProgs

# Other Programs
othProgs <- nvs2018$OTHERPROG_TEXT
othProgs <- othProgs[!is.na(nvs2018$OTHERPROG_TEXT)]
othProgs <- sort(othProgs)
othProgs

OTHERPROGCODED <- nvs2018$OTHERPROG_TEXT
OTHERPROGCODED <- str_to_upper(OTHERPROGCODED)
OTHERPROGCODED <- OTHERPROGCODED[!is.na(OTHERPROGCODED)]
OTHERPROGCODED <- sort(OTHERPROGCODED)

OTHERPROGCODED
