# @knitr socmedia

library(dplyr)
library(likert)
library(ggplot2)

nvs2018 <- read.csv("data/nvs2018.csv")

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

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
################################################
# Open-ended responses for other Social Media

nvs2018$SMOTHERTXT[nvs2018$SMOTHERTXT == "999" | nvs2018$SMOTHERTXT == "9998"] <- NA
nvs2018$SMOTHERTXT[nvs2018$SMOTHERTXT == "9" | nvs2018$SMOTHERTXT == "99"] <- NA
socMedTXT <- na.omit(nvs2018$SMOTHERTXT)
socMedTXT <- toupper(socMedTXT) #Upper Case
socMedTXT <- gsub("  ", " ", socMedTXT)
socMedTXT <- gsub("ALL TRAILS", "ALLTRAILS", socMedTXT)
#socMedTXT <- gsub("ALL TRAILS APP", "ALLTRAILS", socMedTXT)
socMedTXT <- gsub("E BIRD", "EBIRD", socMedTXT)
socMedTXT <- gsub("E-BIRD", "EBIRD", socMedTXT)
socMedTXT <- gsub("EBIRD.ORRG", "EBIRD", socMedTXT)
socMedTXT <- gsub("EBIRD WEBSITE", "EBIRD", socMedTXT)
socMedTXT <- gsub("E-MAIL", "EMAIL", socMedTXT)
socMedTXT <- gsub("E MAIL", "EMAIL", socMedTXT)
socMedTXT <- gsub("JUST EMAIL", "EMAIL", socMedTXT)
socMedTXT <- gsub("SMUGMUG PHOTO POSTING", "SMUGMUG", socMedTXT)
socMedTXT <- gsub("STRAVA RUNNING APP", "STRAVA", socMedTXT)
socMedTXT <- gsub("WE CHAT", "WECHAT", socMedTXT)
socMedTXT <- gsub("WHATS APP", "WHATSAPP", socMedTXT)
#socMedTXT <- gsub("TRIP ADVISOR", "TRIPADVISOR", socMedTXT)
socMedTXT <- sort(socMedTXT)
socMedTXT
