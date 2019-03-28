# ETHNICITY
# Multiple Dichotomous Variables

nvs2018 <- read.csv("data/nvs2018.csv")

library(tidyr)
library(dplyr)
library(likert)
source("code/functions/round_df.R")

str(nvs2018$WHITE)

ethnicity <- subset(nvs2018, select = c(WHITE:OTHERETH))
ethnicity <- na.omit(ethnicity)
str(ethnicity)
#ethnicity[ethnicity == "9"] <- NA

names(ethnicity) = c(
  "White",
  "Hispanic, Latino, or Spanish",
  "Black or African American",
  "Asian",
  "American Indian or Alaska Native",
  "Middle Eastern or North African",
  "Native Hawaiian or Other Pacific Islander",
  "Some other race or ethnicity"
)


dichotlevels <- c('No', 'Yes')

# recode each factor and explicitly set the levels
for (i in seq_along(ethnicity)) {
  ethnicity[, i] <- factor(ethnicity[,i], levels = dichotlevels)
}
ethProp <- likert(ethnicity)
ethProp
ethTitle <- "Ethnicity"
#plot(ethProp, centered=FALSE) + ggtitle(ethTitle)

ethTable <- summary(ethProp)
str(ethTable)

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

ethTable$high <- round_df(ethTable$high, 0)

ethTable[with(ethTable, order(-high)),] %>% select (Item, high)


###################
# race - single variable
eth <- subset(nvs2018, select = c(WHITE:OTHERETH))
str(eth)

eth[eth == "9"] <- NA

eth$newvar <- rowSums(eth == "Yes")
eth$Race <- NA
eth$Race[eth$newvar > 1] <- "Two or more races"
eth$Race[eth$newvar == 1 & eth$WHITE == "Yes"] <- "White"
eth$Race[eth$newvar == 1 & eth$HISPANIC == "Yes"] <- "Hispanic/Latino"
eth$Race[eth$newvar == 1 & eth$AFRAMER == "Yes"] <- "African American/Black"
eth$Race[eth$newvar == 1 & eth$ASIAN == "Yes"] <- "Asian"
eth$Race[eth$newvar == 1 & eth$MIDEAST == "Yes"] <- "Middle Eastern"
eth$Race[eth$newvar == 1 & eth$PACISL == "Yes"] <- "Pacific Islander"
eth$Race[eth$newvar == 1 & eth$OTHERETH == "Yes"] <- "Other"
table(eth$Race)

race <- table(eth$Race)
names(race) = c(
  "White",
  "Hispanic, Latino, or Spanish",
  "Black or African American",
  "Asian",
  "American Indian or Alaska Native",
  "Middle Eastern or North African",
  "Native Hawaiian or Other Pacific Islander",
  "Some other race or ethnicity"
)
###################

ETH$Race <- NA
ETH$Race <- rowSums(ETH == "Yes")
ETH$Race[ETH$newvar > 1] <- "Two or more races"
ETH$Race[ETH$newvar == 1 & ETH$WHITE == "Yes"] <- "White"
ETH$Race[ETH$newvar == 1 & ETH$HISPANIC == "Yes"] <- "Hispanic/Latino"
ETH$Race[ETH$newvar == 1 & ETH$AFRAMER == "Yes"] <- "African American/Black"
ETH$Race[ETH$newvar == 1 & ETH$ASIAN == "Yes"] <- "Asian"
ETH$Race[ETH$newvar == 1 & ETH$MIDEAST == "Yes"] <- "Middle Eastern"
ETH$Race[ETH$newvar == 1 & ETH$PACISL == "Yes"] <- "Pacific Islander"
ETH$Race[ETH$newvar == 1 & ETH$OTHERETH == "Yes"] <- "Other"
table(ETH$Race)

raceProp <- prop.table(table(ETH$Race))*100 # cell percentages
raceProp <- round_df(raceProp) # cell percentages
raceProp <- as.data.frame(raceProp)
raceProp <- raceProp[order(raceProp$Freq, decreasing = TRUE),]
colnames(raceProp) <- c("Race", "Proportion")
raceProp

raceProp <- raceProp %>%
  mutate(Eth = "Race")

library(tidyverse)
library(ggthemes)

ggplot(raceProp, aes(x = ETH, y = Proportion, fill = Race)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  theme_economist(base_size = 1) +
  scale_fill_economist() +
  #  theme_minimal(base_size = 16) +
  #  scale_fill_brewer(palette = "Set2") +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  theme(axis.title.y = element_text(margin = margin(r = 20))) +
  ylab("Percentage") +
  xlab("Race")

ggplot(ETH, aes(x = ETH, y = Proportion, fill = Race)) +
  geom_col() +
  geom_text(aes(label = paste0(Proportion, "%")),
            position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 16) +
  ylab("Percentage") +
  xlab(NULL)

###############
library(tidyr)
library(dplyr)

# Add individual ID to each row
p = mutate(p, id = 1:n())

p %>%
  mutate(p.hispanic = ifelse(p.hispanic == "No", NA, "Hispanic or Latino")) %>% # change p.hispanic column
  gather(category, answer, p.hispanic:p.race_other, na.rm = TRUE) %>%
  filter(answer != "") %>% # get rid of blanks (if were NA would have removed in "gather")
  group_by(id) %>%
  # Create new variable p.race and p.pop based on rules
  mutate(p.race = ifelse(n_distinct(answer) > 1, "Two or more races", answer),
         p.poc = as.integer(p.race == "White, European, Middle Eastern, or Caucasian")) %>%
  slice(1) %>% # take only 1 record for the duplicate id's
  select(-category, - answer) %>% # remove columns that aren't needed
  left_join(p, ., by = "id") %>% # join new columns with original dataset
  select(-id) # remove ID column if not wanted

