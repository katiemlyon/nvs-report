# @knitr info-source

require(dplyr)
require(likert)

nvs2018 <- read.csv("data/nvs2018.csv")


# function to round percentages to whole number
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

##### Information Sources

infoSource <- subset(nvs2018, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
str(infoSource)

infoSource

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems <- subset(nvs2018, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
str(infoItems)

infoItems[infoItems[,ncol(infoItems)] == 6,]
str(infoItems)

infoItems[infoItems=="6"] <- NA #OSU data cleaning?
infoItems[infoItems=="0"] <- NA
infoItems[infoItems=="9"] <- NA
#infoItems <- na.omit(infoItems)
#infoItems <- mutate_if(infoItems, is.numeric, as.factor)
infoItems <- data.frame(apply(infoItems, 2, as.factor))
str(infoItems)

names(infoItems) = c(
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

#infolevels <- c('Did not use', 'Not at all helpful', 'Slightly helpful', 'Moderately helpful', 'Very helpful', 'Extremely helpful')
infolevels <- c('Not at all helpful',
                'Slightly helpful',
                'Moderately helpful',
                'Very helpful',
                'Extremely helpful')

tryCatch({
  # This will throw an error because all the infoItems must have the same number of levels.
  lbad <- likert(infoItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})

sapply(infoItems, class) #Verify that all the columns are indeed factors
sapply(infoItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(infoItems)) {
  infoItems[,i] <- factor(infoItems[,i], levels = infolevels)
}

infoSum <- likert(infoItems)
infoSum
summary(infoSum)

# create table of information sources from summary statistics
infoTable <- summary(infoSum)
str(infoTable)

# round to whole number
infoTable$high <- round_df(infoTable$high, 0)

# sort information sources by most helpful
infoTable[with(infoTable, order(-high)), ] %>% dplyr::select (Item, high)

plot(infoSum)


## Other Website
nvs2018$OTHERWEBTEXT[nvs2018$OTHERWEBTEXT=="99" | nvs2018$OTHERWEBTEXT=="999"] <- NA
otherWeb <- na.omit(nvs2018$OTHERWEBTEXT)
otherWeb <- toupper(otherWeb) #Upper Case
otherWeb <- gsub("  ", " ", otherWeb)
otherWeb <- gsub("HTTPS://WWW.", "", otherWeb)
otherWeb <- gsub("HTTPS://.", "", otherWeb)
otherWeb <- gsub("HTTP://WWW.", "", otherWeb)
otherWeb <- gsub("HTTP://", "", otherWeb)
otherWeb <- gsub("WWW.", "", otherWeb)
otherWeb <- gsub("ALL TRAILS", "ALLTRAILS", otherWeb)
#otherWeb <- gsub("ALL TRAILS APP", "ALLTRAILS", otherWeb)
otherWeb <- gsub("ALLTRAILS APP", "ALLTRAILS", otherWeb)
otherWeb <- gsub("ALLTRAILS.COM", "ALLTRAILS", otherWeb)
otherWeb <- gsub("ALLTRAILS/US", "ALLTRAILS", otherWeb)
otherWeb <- gsub("AUDOBON", "AUDUBON", otherWeb)
otherWeb <- gsub("BIRD AND HIKE.COM", "BIRDANDHIKE.COM", otherWeb)
otherWeb <- gsub("BIRD AND HIKE", "BIRDANDHIKE.COM", otherWeb)
otherWeb <- gsub("E BIRD", "EBIRD", otherWeb)
otherWeb <- gsub("E-BIRD", "EBIRD", otherWeb)
otherWeb <- gsub("EBIRD.COM", "EBIRD", otherWeb)
otherWeb <- gsub("EBIRD.ORG", "EBIRD", otherWeb)
otherWeb <- gsub("TRIP ADVISOR", "TRIPADVISOR", otherWeb)
otherWeb <- sort(otherWeb)
otherWeb

## Other Source
nvs2018$OTHERSOURCETEXT[nvs2018$OTHERSOURCETEXT=="99" | nvs2018$OTHERSOURCETEXT=="999"] <- NA
otherSource <- nvs2018$OTHERSOURCETEXT
otherSource <- na.omit(nvs2018$OTHERSOURCETEXT)
otherSource <- toupper(otherSource) #Upper Case
otherSource <- gsub("  ", " ", otherSource)
#otherSource <- gsub("ALL TRAILS", "ALLTRAILS", otherSource)
otherSource <- gsub("ALL TRAILS APP", "ALLTRAILS", otherSource)
#otherSource <- gsub("AUDOBON", "AUDUBON", otherSource)
otherSource <- gsub("E BIRD", "EBIRD", otherSource)
otherSource <- gsub("E-BIRD", "EBIRD", otherSource)
otherSource <- gsub("EBIRD DATA", "EBIRD", otherSource)
otherSource <- gsub("EBIRD WEBSITE", "EBIRD", otherSource)
therSource <- gsub("LOCAL NEWSPAPER", "NEWSPAPER", otherSource)
therSource <- gsub("NEWS PAPER", "NEWSPAPER", otherSource)
#otherSource <- gsub("TRIP ADVISOR", "TRIPADVISOR", otherSource)
otherSource <- sort(otherSource)
otherSource

## info18to34
nvs18to34 <- nvs2018[which(nvs2018$AGECAT == "18-34 years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems18 <- subset(nvs18to34, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems18[infoItems18=="6"] <- NA #OSU data cleaning?
infoItems18[infoItems18=="0"] <- NA
infoItems18[infoItems18=="9"] <- NA
#infoItems18 <- mutate_if(infoItems18, is.numeric, as.factor)
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
  infoItems18[,i] <- factor(infoItems18[,i], levels=infolevels)
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

plot(infoSum18)

## Other Website
otherWeb <- nvs18to34$OTHERWEBTEXT
otherWeb <- otherWeb[!is.na(nvs18to34$OTHERWEBTEXT)]
otherWeb

## Other Source
otherSource <- nvs18to34$OTHERSOURCETEXT
otherSource <- otherSource[!is.na(nvs18to34$OTHERSOURCETEXT)]
otherSource

## info35plus
nvs35plus <- nvs2018[which(nvs2018$AGECAT == "35-49 years" | nvs2018$AGECAT == "50-64 years" | nvs2018$AGECAT == "65+ years"), ]

##### How helpful are the information sources?
infoTitle <- "How helpful are the information sources?"

infoItems35plus <- subset(nvs35plus, select = c(PREVKNOW:OTHERWEB, SOCMINFO:OTHERSOURCE))
infoItems35plus[infoItems35plus=="6"] <- NA #OSU data cleaning?
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
  infoItems35plus[,i] <- factor(infoItems35plus[,i], levels=infolevels)
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

# Social Media

# Social Media - All
sm <- subset(nvs2018, select = c(SMFACE:SMNONE, AGECAT))
sm[sm=="9"] <- NA
smItems <- subset(nvs2018, select = c(SMFACE:SMNONE))
smItems[smItems=="9"] <- NA
#smItems <- mutate_if(smItems, is.numeric, as.factor)
smItems <- data.frame(apply(smItems, 2, as.factor))
str(smItems)
names(smItems) = c(
  "Facebook",
  "Flickr",
  "Instagram",
  "Pinterest",
  "Snaphat",
  "Twitter",
  "Vimeo",
  "YouTube",
  "Blog",
  "Travel Website",
  "Other",
  "None"
)
dichotlevels <- c('No', 'Yes')
# Here we will recode each factor and explicitly set the levels
for(i in seq_along(smItems)) {
  smItems[,i] <- factor(smItems[,i], levels = dichotlevels)
}
smProp <- likert(smItems)
smProp
smTitle <- "Social Media"
#plot(smProp, centered=FALSE) + ggtitle(smTitle)
smTable <- summary(smProp)
str(smTable)

smTable$high <- round_df(smTable$high, 0)
smTable[with(smTable, order(-high)), ] %>% select (Item, high)

# Social Media - Under 35
nvs18to34 <- nvs2018[which(nvs2018$AGECAT == "18-34 years"), ]
smItems18to34 <- subset(nvs18to34, select = c(SMFACE:SMNONE))
smItems18to34[smItems18to34=="9"] <- NA
smItems18to34 <- data.frame(apply(smItems, 2, as.factor))
str(smItems18to34)
names(smItems18to34) = c(
  "Facebook",
  "Flickr",
  "Instagram",
  "Pinterest",
  "Snaphat",
  "Twitter",
  "Vimeo",
  "YouTube",
  "Blog",
  "Travel Website",
  "Other",
  "None"
)
dichotlevels <- c('No', 'Yes')
# Here we will recode each factor and explicitly set the levels
for(i in seq_along(smItems18to34)) {
  smItems18to34[,i] <- factor(smItems18to34[,i], levels = dichotlevels)
}
smProp18to34 <- likert(smItems18to34)
smProp18to34
smTitle18to34 <- "Social Media for Under 35"
#plot(smProp18to34, centered=FALSE) + ggtitle(smTitle)
smTable18to34 <- summary(smProp18to34)
str(smTable18to34)
round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}
smTable18to34$high <- round_df(smTable18to34$high, 0)
smTable18to34[with(smTable18to34, order(-high)), ] %>% select (Item, high)

## subset Social Media variables
sm <- subset(nvs2018, select = c(SMFACE:SMNONE))
#sm[sm=="9"] <- NA

str(sm)
levels(sm$SMNONE)
sm$SMNONE <- droplevels(sm$SMNONE, exclude = "4") #OSU clean
sm$SMNONE <- droplevels(sm$SMNONE, exclude = "5") #OSU clean
sm$SMNONE <- droplevels(sm$SMNONE, exclude = "999") #OSU clean

# calculate percentages with function
tblFun <- function(x){
  tbl <- table(x)
  res <- cbind(tbl,round(prop.table(tbl)*100))
  colnames(res) <- c('Count','Percentage')
  res
}

do.call(rbind,lapply(sm[1:12],tblFun))


# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(sm, aes(x = reorder_size(`SMFACE`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("State or Province") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  #facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####################

#library(expss)
# add value lables for preserving empty categories
#val_lab(sm) = autonum(1:2)
#res = sm
#for(each in colnames(sm)){
#  res = res %>%
#    tab_cells(list(each)) %>%
#    tab_cols(vars(each)) %>%
#    tab_stat_rpct(total_row_position = "none")
#}

#res = res %>% tab_pivot()

# add percentage sign
#recode(res[,-1]) = other ~ function(x) ifelse(is.na(x), NA, paste0(round(x, 0), "%"))
#res

##################

#sm$sumSM <- rowSums (sm, na.rm = F, 1)

##################
#table(sm)
#round(100*prop.table(sm),2) # Tot proportions rounded
#########################

smTitle <- "Social Media"

smItems <- subset(nvs2018, select = c(SMFACE:SMNONE))
smItems[smItems=="9"] <- NA
#smItems <- mutate_if(smItems, is.numeric, as.factor)
smItems <- data.frame(apply(smItems, 2, as.factor))
str(smItems)

names(smItems) = c(
  "Facebook",
  "Flickr",
  "Instagram",
  "Pinterest",
  "Snaphat",
  "Twitter",
  "Vimeo",
  "YouTube",
  "Blog",
  "Travel Website",
  "Other",
  "None"
)

dichotlevels <- c('No', 'Yes')

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(smItems)) {
  smItems[,i] <- factor(smItems[,i], levels = dichotlevels)
}
smProp <- likert(smItems)
smProp
#plot(smProp, centered=FALSE) + ggtitle(smTitle)

smTable <- summary(smProp)
str(smTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

smTable$high <- round_df(smTable$high, 0)

smTable[with(smTable, order(-high)), ] %>% select(Item, high)


################################################
# More/About/Less than half use social media
smNone <- prop.table(table(na.omit(nvs2018$SMNONE)))*100
#smNone <- data.frame(smNone)
str(smNone)

smNoneNo <- smNone["No"]
useSocmed <- as.numeric(smNoneNo)
useSocmed <- round_df(useSocmed, 0)
useSocmed #percent that did use social media

useSocmedTxt <- if(useSocmed <48){"Less than "}else if(useSocmed >= 48 & useSocmed <= 52){"About "}else if(useSocmed > 52){"More than "}
useSocmedTxt

