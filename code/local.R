# Local/Nonlocal composition

#==============
# LOAD PACKAGES
#==============
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyverse)
library(tidyr)

nvs2018 <- read.csv("data/nvs2018.csv")
levels(nvs2018$LOCALAREA)
nvs2018$LOCALAREA <- factor(nvs2018$LOCALAREA)
levels(nvs2018$LOCALAREA)

nvs2018 %>%
  select(LOCALAREA) %>%
  drop_na(LOCALAREA) -> local
table(local)

#--------
# INSPECT
#--------
local %>% glimpse()
local %>% count(LOCALAREA)

#========================
# Local/Nonlocal Overview
#========================

## Local/Nonlocal
local <- nvs2018$LOCALAREA
str(local)

localTable <- table(local)
localTable

propLocal = round_df((localTable)["Local"]/sum(localTable)*100,0)
propLocal

propNonlocal = round_df((localTable)["Nonlocal"]/sum(localTable)*100,0)
propNonlocal

#=================
# SIMPLE BAR CHART
#=================

ggplot(data = local, aes(x = LOCALAREA)) +
  geom_bar()

#===========================================
# SIMPLE BAR CHART (USING THE PIPE OPERATOR)
#===========================================

local %>%
  ggplot(aes(x = LOCALAREA)) +
  geom_bar()


gg <- ggplot(data = localNonlocal, aes(x=localNonlocal))

ggLocal <- gg +
  geom_bar(data=localNonlocal,
           aes( y = ..count../sum(..count..), fill = localNonlocal)) +
  coord_flip() +
  theme_bw()
ggLocal


#######################
# donut

local <- as.data.frame(local)

#--------
# INSPECT
#--------
local %>% glimpse()
local %>% count(local)

dLocal <- table(local)

local %>%
  group_by(LOCALAREA) %>%
  summarise(count = length(LOCALAREA))

local2 <- local %>%
  count(LOCALAREA) %>%
  mutate(LOCALAREA = factor(LOCALAREA, levels = unique(LOCALAREA)))

# ----------------
# Basic pie chart
# ----------------

# Add addition columns, needed for drawing with geom_rect.
local2$fraction = local2$n / sum(local2$n)
local2 = local2[order(local2$fraction), ]
local2$ymax = cumsum(local2$fraction)
local2$ymin = c(0, head(local2$ymax, n=-1))


# Make the donut plot
p1 = ggplot(local2, aes(fill=LOCALAREA, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "% Local") +
  labs(title="")
p1


# ----------------
# Interactive pie chart
# ----------------

library(dplyr)
library(ggplot2)
library(ggiraph)

donut_data <- local2 %>%
  mutate(
    percentage = n / sum(n),
    hover_text = paste0(LOCALAREA, ": ", n)
  ) %>%
  mutate(percentage_label = paste0(round(100 * percentage, 0), "%"))

donut_plot <- ggplot(donut_data, aes(y = n, fill = LOCALAREA)) +
  geom_bar_interactive(
    aes(x = 1, tooltip = hover_text),
    width = 0.1,
    stat = "identity",
    show.legend = FALSE
  ) +
  annotate(
    geom = "text",
    x = 0,
    y = 0,
    label = donut_data[["percentage_label"]][donut_data[["LOCALAREA"]] == "Local"],
    size = 30,
    color = "dark blue"
  ) +
  scale_fill_manual(values = c(Local = "dark blue", Nonlocal = "light blue")) +
  coord_polar(theta = "y") +
  theme_void()

ggiraph(ggobj = donut_plot)


########

