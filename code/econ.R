## @knitr econ
#nvs2018 <- read.csv("C:/Users/klyon/hd_visitorsurvey/nvs-reports/data/nvs2018.csv")

#library(plyr)
library(dplyr)
library(tidyr)
library(likert)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(scales)
#library(waffle)
#library(extrafont)

#load functions
source("code/functions/calc_pct.R")
source("code/functions/round_df.R")

#read data
nvs2018 <- read.csv("data/nvs2018.csv")

##### WTP
# wtpTitle <- "If your total trip costs were to increase, what is the maximum extra amount you would pay and still visit this refuge?""
