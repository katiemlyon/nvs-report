# @knitr comments

nvs2018 <- read.csv("../data/nvs2018.csv")
library(dplyr)
#library(tidyr)
library(likert)
#library(waffle)
#library(extrafont)
#library(ggplot2)

# Open-ended responses for other visitor center activities
otherVC <- na.omit(nvs2018$OTHERVISTXT)
otherVC

################################

#transComments <- nvs2018$TRANSCOMTXT
transComments <- na.omit(nvs2018$TRANSCOMTXT)
transComments <- gsub("  ", " ", transComments)
transComments <- as.data.frame(transComments, colnames = FALSE)
transComments <- transComments[order(transComments),]

# save data as csv file
write.csv(transComments,
          file = "../data/nvs2018-transportation-comments.csv",
          row.names = FALSE,
          na = "")

################################

# Comments on Services, Facilities, Opportunities

sfoComments <- nvs2018$SERVCOMTXT
sfoComments <- na.omit(nvs2018$SERVCOMTXT)
sfoComments <- gsub("  ", " ", sfoComments)
#sfoComments <- toupper(sfoComments) #Upper Case
#sfoComments <- str_to_sentence(sfoComments) #Sentence Case
sfoComments <- as.data.frame(sfoComments, colnames = FALSE)
sfoComments <- sfoComments[order(sfoComments),]
#sfoComments

# save data as csv file
write.csv(sfoComments,
          file = "../data/nvs2018-service-comments.csv",
          row.names = FALSE,
          na = "")

################################

# General Comments

# create comment variable and drop NAs
endComments <- na.omit(nvs2018$CMNTSTXT)
endComments <- gsub("  ", " ", endComments)
endComments <- gsub("  ", " ", endComments)
endComments <- gsub("  ", " ", endComments)
endComments <- as.data.frame(endComments, colnames = FALSE)
endComments <- endComments[order(endComments),]
length(endComments)

# save data as csv file
write.csv(endComments,
          file = "../data/nvs2018-comments.csv",
          row.names = FALSE)

