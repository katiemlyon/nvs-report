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
eth$Race[eth$newvar == 1 & eth$HISPANIC == "Yes"] <- "Hispanic, Latino, or Spanish"
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



raceProp <- prop.table(table(eth$Race))*100 # cell percentages
raceProp <- round_df(raceProp) # cell percentages
raceProp <- as.data.frame(raceProp)
raceProp <- raceProp[order(raceProp$Freq, decreasing = TRUE),]
colnames(raceProp) <- c("Race", "Proportion")
raceProp

raceProp <- raceProp %>%
  mutate(Eth = "Race")

race1 <- raceProp$Race[1]
race1Prop <- raceProp$Proportion[1]
race2 <- raceProp$Race[2]
race2Prop <- raceProp$Proportion[2]
race3 <- raceProp$Race[3]
race3Prop <- raceProp$Proportion[3]

propHispanic <- raceProp$Proportion[raceProp$Race=="Hispanic, Latino, or Spanish"]
propHispanic


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

## felt welcome by race

ethWelcome <- subset(nvs2018, select = c(FELTWEL, WHITE:OTHERETH))

ethWelcome$newvar <- rowSums(select(ethWelcome, c(2:9))  == "Yes", na.rm = TRUE)
range(ethWelcome$newvar)
ethWelcome$Race <- NA
ethWelcome$Race[ethWelcome$newvar > 1] <- "Two or more races"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$WHITE == "Yes"] <- "White"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$HISPANIC == "Yes"] <- "Hispanic, Latino, or Spanish"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$AFRAMER == "Yes"] <- "African American/Black"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$ASIAN == "Yes"] <- "Asian"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$MIDEAST == "Yes"] <- "Middle Eastern"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$PACISL == "Yes"] <- "Pacific Islander"
ethWelcome$Race[ethWelcome$newvar == 1 & ethWelcome$OTHERETH == "Yes"] <- "Other"
table(ethWelcome$Race)

factor(ethWelcome$FELTWEL)
ethWelcome <- na.omit(ethWelcome)


ethWelProp <- ethWelcome %>%
  group_by(FELTWEL, Race) %>%
  summarise(n = n())
