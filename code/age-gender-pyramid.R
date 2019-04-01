library(XML)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(plyr)
library(gridExtra)


# read data
nvs2018 <- read.csv("data/nvs2018.csv")

# Pyramid Plot
# https://stackoverflow.com/questions/4559229/drawing-pyramid-plot-using-r-and-ggplot2
require(ggplot2)
df <- data.frame(Type = sample(c('Male', 'Female', 'Female'), 1000, replace=TRUE),
                 Age = sample(18:60, 1000, replace=TRUE))

df$Age <- AgesFactor

# create a data frame
pyramidDF <- subset(nvs2018,
       select = c(AGE, GENDER))
pyramidDF <- na.omit(pyramidDF)
str(pyramidDF)

# convert Age column to a factor with the required break-points:
AgesFactor <- ordered( cut(pyramidDF$AGE, breaks = c(18,seq(35,100,15)),
                           include.lowest = TRUE))

levels(AgesFactor)

pyramidDF$AGE <- AgesFactor

# start building the plot:
# create the male and female plots with the corresponding subset of the data, suppressing legends, etc.

gg <- ggplot(data = pyramidDF, aes(x=AGE))

gg.male <- gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Male',],
            aes( y = ..count../sum(..count..), fill = AGE)) +
  coord_flip() +
  theme_bw()
gg.male

# For the female plot, reverse the 'Percent' axis using trans = "reverse"
gg.female <- gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Female',],
           aes( y = ..count../sum(..count..), fill = AGE)) +
  scale_y_continuous(trans = 'reverse') +
  coord_flip() +
  theme_bw()
gg.female

# Now create a plot just to display the age-brackets using geom_text,
# but also use a dummy geom_bar to ensure that the scaling of the "age" axis
# in this plot is identical to those in the male and female plots:

gg.ages <- gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Male',],
           aes( y = 0, fill = alpha('white',0))) +
  geom_text( aes( y = 0,  label = as.character(AGE)), size = 3) +
  coord_flip()
gg.ages

## Plotting
gg <- ggplot(data = pyramidDF, aes(x=AGE))

gg.male <- gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Male',],
            aes( y = ..count../sum(..count..), fill = AGE)) +
  scale_y_continuous('', labels = scales::percent) +
  theme(legend.position = 'none',
        axis.text.y = theme_bw()$axis.text.y,
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11.5),
        plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm")) +
  ggtitle("Male") +
  coord_flip()

gg.female <-  gg +
  geom_bar(data=pyramidDF[pyramidDF$GENDER == 'Female',],
            aes( y = ..count../sum(..count..), fill = AGE)) +
  scale_y_continuous('', labels = scales::percent,
                     trans = 'reverse') +
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(size = 11.5),
        plot.margin=unit(c(0.1,0,0.1,0.05),"cm")) +
  ggtitle("Female") +
  coord_flip() +
  ylab("Age")

## Plutting it together
agePyramid <- grid.arrange(gg.female,
             gg.male,
             widths=c(0.4,0.6),
             ncol=2
)

