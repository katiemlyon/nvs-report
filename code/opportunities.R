## @knitr recopps
require(likert)

##### Recreation Opportunities
#BIRDWATIMP	WILDWATIMP	PHOTOIMP	ENVIEDIMP	HUNTIMP	FISHIMP	TRHIKEIMP	BIKEIMP	WATTRAILIMP	VOLUNIMP WILDIMP	
#BIRDWATSAT	WILDWATSAT	PHOTOSAT	ENVIEDSAT	HUNTSAT	FISHSAT	TRHIKESAT	BIKESAT	WATTRAILSAT	VOLUNSAT	WILDSAT

##### Importance of recreation opportunities

recImpTitle <- "How important are the recreation opportunities?"

recImpItems <- subset(nvs2018, select = c(BIRDWATIMP:WILDIMP))
recImpItems[recImpItems=="9"] <- NA
#recImpItems <- mutate_if(recImpItems, is.numeric, as.factor)
recImpItems <- data.frame(apply(recImpItems, 2, as.factor))
str(recImpItems)

implevels <- c('Not at all important',
                'Slightly important',
                'Moderately important',
                'Very important',
                'Extremely important')

tryCatch({
  # This will throw an error because all the oppImpItems must have the same number of levels.
  lbad <- likert(recImpItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(recImpItems, class) #Verify that all the columns are indeed factors
sapply(recImpItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(recImpItems)) {
  recImpItems[,i] <- factor(recImpItems[,i], levels = implevels)
}

recImp <- likert(recImpItems)
recImp
summary(recImp)

plot(recImp, centered=TRUE) + ggtitle(recImpTitle)


##### Satisfaction with recreation opportunities
recSatTitle <- "How satisfied are you with the recreation opportunities?"

recSatItems <- subset(nvs2018, select = c(HOURSSAT:WILDSAT))
str(recSatItems)
recSatItems <- droplevels.data.frame(recSatItems, exclude = "6")
recSatItems <- droplevels.data.frame(recSatItems, exclude = NA)
str(recSatItems)

#recSatItems[recSatItems=="0"] <- NA
#recSatItems[recSatItems=="9"] <- NA
sum(rowSums(is.na(recSatItems[,1:18]))==18) #number of people skipped

#recSatItems <- mutate_if(recSatItems, is.numeric, as.factor)
recSatItems <- data.frame(apply(recSatItems, 2, as.factor))
str(recSatItems)

satlevels <- c('Not at all satisfied',
                'Slightly satisfied',
                'Moderately satisfied',
                'Very satisfied',
                'Extremely satisfied')

tryCatch({
  # This will throw an error because all the recSatItems must have the same number of levels.
  lbad <- likert(recSatItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(recSatItems, class) #Verify that all the columns are indeed factors
sapply(recSatItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(recSatItems)) {
  recSatItems[,i] <- factor(recSatItems[,i], levels = satlevels)
}

recSat <- likert(recSatItems)
recSat
summary(recSat)

plot(recSat, centered=TRUE) + ggtitle(recSatTitle)
plot(recSat, centered=FALSE) + ggtitle(recSatTitle)

