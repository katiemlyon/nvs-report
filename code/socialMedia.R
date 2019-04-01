# @knitr socmedia

#load packages
library(dplyr)
library(likert)
library(ggplot2)

#load functions
source("code/functions/calc_pct.R")
source("code/functions/round_df.R")

#read data
nvs2018 <- read.csv("data/nvs2018.csv")

##############
# Social Media
##############

################################################
# More/About/Less than half use social media
################################################

smNone <- prop.table(table(na.omit(nvs2018$SMNONE)))*100
#smNone <- data.frame(smNone)
str(smNone)

smNoneNo <- smNone["No"] #people that did not check "do not use"
useSocmed <- as.numeric(smNoneNo)
useSocmed <- round_df(useSocmed, 0)
useSocmed #percent that did use social media

# automates report text
useSocmedTxt <- if(useSocmed <48){"Less than "}else if(useSocmed >= 48 & useSocmed <= 52){"About "}else if(useSocmed > 52){"More than "}
useSocmedTxt

################################################

# Social Media - All
sm <- subset(nvs2018, select = c(SMFACE:SMNONE, AGECAT))
str(sm)

smItems <- subset(nvs2018, select = c(SMFACE:SMNONE))
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

smTable18to34$high <- round_df(smTable18to34$high, 0)
smTable18to34[with(smTable18to34, order(-high)), ] %>% select (Item, high)

## subset Social Media variables
sm <- subset(nvs2018, select = c(SMFACE:SMNONE))
#sm[sm=="9"] <- NA

str(sm)
levels(sm$SMNONE)

# calculate percentages with function
do.call(rbind,lapply(sm[1:12],calc_pct))


# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(sm, aes(x = reorder_size(`SMFACE`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Shared Refuge Experience") +
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

# round
smTable$high <- round_df(smTable$high, 0)

smTable[with(smTable, order(-high)), ] %>% select(Item, high)


################################################
# Social Shares by Channel


