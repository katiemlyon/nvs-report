## @knitr facserv
require(likert)
nvs2018 <- read.csv("data/nvs2018.csv")

#####Facilities, Services, Opportunities#
#HOURSIMP	AVAILEMPIMP	POLITEIMP	REGSIGNIMP	VISCENIMP	LAVIMP	OBDECKIMP	BIRDWATIMP	WILDWATIMP	PHOTOIMP	ENVIEDIMP	HUNTIMP	FISHIMP	TRHIKEIMP	BIKEIMP	WATTRAILIMP	VOLUNIMP	WILDIMP	
#HOURSSAT	AVAILEMPSAT	POLITESAT	REGSIGNSAT	VISCENSAT	LAVSAT	OBDECKSAT	BIRDWATSAT	WILDWATSAT	PHOTOSAT	ENVIEDSAT	HUNTSAT	FISHSAT	TRHIKESAT	BIKESAT	WATTRAILSAT	VOLUNSAT	WILDSAT

##### Importance of transportation items
servicesImpTitle <- "How important are the items?"

servicesImpItems <- subset(nvs2018, select = c(HOURSIMP:WILDIMP))

# Assign NA to missing values that were coded 9:
servicesImpItems[servicesImpItems==9] <- NA
#servicesImpItems <- mutate_if(servicesImpItems, is.numeric, as.factor)
servicesImpItems <- data.frame(apply(servicesImpItems, 2, as.factor))
str(servicesImpItems)

implevels <- c('Not at all important',
               'Slightly important',
               'Moderately important',
               'Very important',
               'Extremely important')

tryCatch({
  # This will throw an error because all the servicesImpItems must have the same number of levels.
  lbad <- likert(servicesImpItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(servicesImpItems, class) #Verify that all the columns are indeed factors
sapply(servicesImpItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(servicesImpItems)) {
  servicesImpItems[,i] <- factor(servicesImpItems[,i], levels = implevels)
}

servicesImp <- likert(servicesImpItems)
summary(servicesImp)
plot(servicesImp, centered=TRUE) + ggtitle(servicesImpTitle)


##### Satisfaction with transportation items
servicesSatTitle <- "Rate how satisfied you are with the way this refuge is managing each feature"

servicesSatItems <- subset(nvs2018, select = c(HOURSSAT:WILDSAT))
str(servicesSatItems)

servicesSatItems <- droplevels.data.frame(servicesSatItems, exclude = "6")
servicesSatItems <- droplevels.data.frame(servicesSatItems, exclude = NA)
str(servicesSatItems)

#servicesSatItems[servicesSatItems=="0"] <- NA
#servicesSatItems[servicesSatItems=="9"] <- NA
#servicesSatItems <- mutate_if(servicesSatItems, is.numeric, as.factor)
servicesSatItems <- data.frame(apply(servicesSatItems, 2, as.factor))
str(servicesSatItems)

satlevels <- c('Not at all satisfied',
               'Slightly satisfied',
               'Moderately satisfied',
               'Very satisfied',
               'Extremely satisfied')

tryCatch({
  # This will throw an error because all the servicesSatItems must have the same number of levels.
  lbad <- likert(servicesSatItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(servicesSatItems, class) #Verify that all the columns are indeed factors
sapply(servicesSatItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(servicesSatItems)) {
  servicesSatItems[,i] <- factor(servicesSatItems[,i], levels = satlevels)
}

servicesSat <- likert(servicesSatItems)
summary(servicesSat)
plot(servicesSat, centered=TRUE) + ggtitle(servicesSatTitle)
plot(servicesSat, centered=TRUE)

