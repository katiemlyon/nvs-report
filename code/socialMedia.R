# Social Media

#library(dplyr)
library(likert)
library(ggplot2)

#nvs2018 <- readr::read_csv("data/nvs2018.csv")

## subset Social Media variables
sm <- subset(nvs2018, select = c(SMFACE:SMNONE))
sm[sm=="9"] <- NA

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

library(expss)
# add value lables for preserving empty categories
#val_lab(sm) = autonum(1:2)
res = sm
for(each in colnames(sm)){
  res = res %>% 
    tab_cells(list(each)) %>% 
    tab_cols(vars(each)) %>% 
    tab_stat_rpct(total_row_position = "none")
}

res = res %>% tab_pivot()

# add percentage sign
recode(res[,-1]) = other ~ function(x) ifelse(is.na(x), NA, paste0(round(x, 0), "%"))
res

##################

sm$sumSM <- rowSums (sm, na.rm = F, 1)

##################
#table(sm)
round(100*prop.table(sm),2) # Tot proportions rounded
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

smTable[with(smTable, order(-high)), ] %>% select (Item, high)

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
