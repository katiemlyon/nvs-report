require(likert)

##### Facilities, Services, Opportunities
#HOURSIMP	AVAILEMPIMP	POLITEIMP	REGSIGNIMP	VISCENIMP	LAVIMP	OBDECKIMP	
#BIRDWATIMP	WILDWATIMP	PHOTOIMP	ENVIEDIMP	HUNTIMP	FISHIMP	TRHIKEIMP	BIKEIMP	WATTRAILIMP	VOLUNIMP WILDIMP	
#HOURSSAT	AVAILEMPSAT	POLITESAT	REGSIGNSAT	VISCENSAT	LAVSAT	OBDECKSAT	
#BIRDWATSAT	WILDWATSAT	PHOTOSAT	ENVIEDSAT	HUNTSAT	FISHSAT	TRHIKESAT	BIKESAT	WATTRAILSAT	VOLUNSAT	WILDSAT

##### Importance of Facilities, Services, Opportunities

ImpTitle <- "How important are the items?"

ImpItems <- subset(nvs2018, select = c(HOURSIMP:WILDIMP))
ImpItems[ImpItems=="9"] <- NA
sum(rowSums(is.na(ImpItems[,1:18]))==18) #number of people skipped

#ImpItems <- mutate_if(ImpItems, is.numeric, as.factor)
ImpItems <- data.frame(apply(ImpItems, 2, as.factor))
str(ImpItems)

implevels <- c('Not at all important',
               'Slightly important',
               'Moderately important',
               'Very important',
               'Extremely important')

tryCatch({
  # This will throw an error because all the servicesImpItems must have the same number of levels.
  lbad <- likert(ImpItems)
}, error=function(e) { 
  print("This is good that an error was thrown!")
  print(e) 
})

sapply(ImpItems, class) #Verify that all the columns are indeed factors
sapply(ImpItems, function(x) { length(levels(x)) } ) # The number of levels in each factor

# Here we will recode each factor and explicitly set the levels
for(i in seq_along(ImpItems)) {
  ImpItems[,i] <- factor(ImpItems[,i], levels = implevels)
}

fsoImp <- likert(ImpItems)
summary(fsoImp)
plot(fsoImp)
plot(fsoImp, centered=FALSE)

################################################

## Services and Facilities

#####Facilities, Services
#HOURSIMP	AVAILEMPIMP	POLITEIMP	REGSIGNIMP	VISCENIMP	LAVIMP	OBDECKIMP	BIRDWATIMP	WILDWATIMP	PHOTOIMP	ENVIEDIMP	HUNTIMP	FISHIMP	TRHIKEIMP	BIKEIMP	WATTRAILIMP	VOLUNIMP	WILDIMP	
#HOURSSAT	AVAILEMPSAT	POLITESAT	REGSIGNSAT	VISCENSAT	LAVSAT	OBDECKSAT	BIRDWATSAT	WILDWATSAT	PHOTOSAT	ENVIEDSAT	HUNTSAT	FISHSAT	TRHIKESAT	BIKESAT	WATTRAILSAT	VOLUNSAT	WILDSAT

##### Importance of items
servicesImpTitle <- "How important are the items?"

servicesImpItems <- subset(nvs2018, select = c(HOURSIMP:OBDECKIMP))
servicesImpItems[servicesImpItems=="9"] <- NA
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
servicesImp
summary(servicesImp)
plot(servicesImp)

##### Satisfaction with services and facilities
servicesSatTitle <- "Rate how satisfied you are with the way this refuge is managing each feature"

servicesSatItems <- subset(nvs2018, select = c(HOURSSAT:OBDECKSAT))
servicesSatItems[servicesSatItems=="0"] <- NA
servicesSatItems[servicesSatItems=="9"] <- NA
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
servicesSat
summary(servicesSat)

plot(servicesImp, centered=FALSE) + ggtitle(servicesImpTitle)

plot(servicesSat, centered=FALSE) + ggtitle(servicesSatTitle)

################################################

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

################################################
# Gap Analysis
nvsgap <- subset(nvs2018, select = c(HOURSIMP:WILDSAT))
str(nvsgap)
levels(nvsgap$WILDSAT)
nvsgap <- droplevels.data.frame(nvsgap, exclude = "6")
nvsgap <- droplevels.data.frame(nvsgap, exclude = NA)
str(nvsgap)
implevels <- c('Not at all important',
               'Slightly important',
               'Moderately important',
               'Very important',
               'Extremely important')

l.sat <- likert(items=nvsgap[,19:36], nlevels=5)
#l.sat <- likert(items=gap[,(HOURSSAT:WILDSAT], nlevels=5)
plot(l.sat, centered=TRUE) + ggtitle('Satisfaction')


l.imp <- likert(items=nvsgap[,1:18], nlevels=5)
#l.imp <- likert(items=impItems, nlevels=5)
plot(l.imp, centered=FALSE) + ggtitle('Importance')

l.gap <- likert(items=nvsgap[,19:36], importance=nvsgap[,1:18], nlevels=5)
#l.gap <- likert(items=satItems, importance=impItems, nlevels=5)
class(l.gap)

summary(l.gap)
print(l.gap, row.names=FALSE)

#plot(l.gap) # Default is bar
plot(l.gap, type='density')

#plot
plot(l.gap, type = "density",
     include.histogram = TRUE, 
     panel.widths = c(3, 1), 
     panel.arrange = "v",
     panel.strip.color = "#F0F0F0", 
     legend.position = "bottom",
     panel.background = element_rect(size = 1, color = "grey70", fill = NA),
     satisfaction.label = "Satisfaction", 
     importance.label = "Importance")

# Gap Analysis
nvsgap <- subset(nvs2018, select = c(HOURSIMP:WILDSAT))
nvsgap[nvsgap==9] <- NA

#impItems <- subset(nvs2018, select = c(HOURSIMP:WILDIMP))
#impItems[impItems==9] <- NA

l.sat <- likert(items=nvsgap[,19:36], nlevels=5)
#l.sat <- likert(items=gap[,(HOURSSAT:WILDSAT], nlevels=5)
plot(l.sat, centered=FALSE) + ggtitle('Satisfaction')


l.imp <- likert(items=nvsgap[,1:18], nlevels=5)
#l.imp <- likert(items=impItems, nlevels=5)
plot(l.imp, centered=FALSE) + ggtitle('Importance')

l.gap <- likert(items=nvsgap[,19:36], importance=nvsgap[,1:18], nlevels=5)
#l.gap <- likert(items=satItems, importance=impItems, nlevels=5)
class(l.gap)

summary(l.gap)
print(l.gap, row.names=FALSE)

#plot(l.gap) # Default is bar
plot(l.gap, type='density')

#plot
plot(l.gap, type = "density",
     include.histogram = TRUE, 
     #panel.widths = c(3, 1), 
     #panel.arrange = "v",
     #panel.strip.color = "#F0F0F0", 
     legend.position = "bottom",
     panel.background = element_rect(size = 1, color = "grey70", fill = NA),
     satisfaction.label = "Satisfaction", 
     importance.label = "Importance")



