require(shiny)
require(devtools)
require(likert)
require(ggplot2)


nvs2018 <- read.csv("nvs2018.csv")
nlevels <-
  c('Strongly Disagree',
    'Disagree',
    'Neither',
    'Agree',
    'Strongly Agree')

##### Safe and Welcome
#title <- "Safe and Welcome"

safeItems <- subset(nvs2018, select = c(FELTWEL:CRIMEPROB))
#safeItems[safeItems == "9"] <- NA
safeItems <- data.frame(apply(safeItems, 2, as.factor))
safeItems <- droplevels(safeItems, exclude = "")

str(safeItems)


names(safeItems) = c(
  "I felt welcome during my visit to this refuge.",
  "I felt safe during my visit to this refuge.",
  "Crime is a problem at this refuge."
)

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(safeItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})
sapply(safeItems, class) #Verify that all the columns are indeed factors
sapply(safeItems, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(safeItems)) {
  safeItems[,i] <- factor(safeItems[,i], levels=nlevels)
}

safe = likert(safeItems)
safe
plot(safe)

ageSafe <- likert(safeItems, grouping = nvs2018$AGECAT) # Group by age
plot(ageSafe)

hispanicSafe <- likert(safeItems, grouping = nvs2018$HISPANIC)
plot(hispanicSafe)

whiteSafe <- likert(safeItems, grouping = nvs2018$WHITE)
plot(whiteSafe)

blackSafe <- likert(safeItems, grouping = nvs2018$AFRAMER)
plot(blackSafe)

asianSafe <- likert(safeItems, grouping = nvs2018$ASIAN)
plot(asianSafe)

refugeSafe <- likert(safeItems, grouping = nvs2018$RefugeName)
plot(refugeSafe)

##### Comfort in Nature
#title <- "Comfort in Nature"

comfortItems <- subset(nvs2018, select = c(COMFORT:TREATDIF))
#comfortItems[comfortItems == "9"] <- NA
comfortItems <- data.frame(apply(comfortItems, 2, as.factor))
comfortItems <- droplevels(comfortItems, exclude = "")

str(comfortItems)


names(comfortItems) = c(
  "I feel comfortable being in nature.",
  "I do not like being in nature by myself.",
  "People closest to me enjoy participating in nature-based recreation.",
  "Generally, people who look like me are treated differently when they participate in nature-based recreation."
)

tryCatch({
  # This will throw an error because all the items must have the same number of levels.
  lbad <- likert(comfortItems)
}, error=function(e) {
  print("This is good that an error was thrown!")
  print(e)
})
sapply(comfortItems, class) #Verify that all the columns are indeed factors
sapply(comfortItems, function(x) { length(levels(x)) } ) # The number of levels in each factor
for(i in seq_along(comfortItems)) {
  comfortItems[,i] <- factor(comfortItems[,i], levels=nlevels)
}

comfort = likert(comfortItems)
comfort
plot(comfort)

age <- likert(comfortItems, grouping = nvs2018$AGECAT)
plot(age)

Aggregate <- likert(comfortItems)
plot(Aggregate)


lcomfortg <- likert(comfortItems, grouping = nvs2018$AGECAT) # Group by age




# Define server logic
shinyServer(
  function(input, output)
{
  # Return the requested dataset #TODO have this switch between items
  datasetInput <- reactive
  ({
    switch(input$dataset,
           "Safe and Welcoming" = safe,
           "Comfort in Nature" = nature)
  })
  
  # Generate a summary of the dataset
  output$percent<-renderTable({
    dataset<-datasetInput()
    print(dataset)
  },
  include.rownames = FALSE)
  
  output$stats <- renderTable({
    dataset <- datasetInput()
    xtab <- xtable(
      dataset,
      caption = input$caption,
      include.n = input$include.n,
      include.mean = input$include.mean,
      include.sd = input$include.sd,
      include.low = input$include.low,
      include.neutral = input$include.neutral,
      include.high = input$include.high,
      include.missing = input$include.missing,
      #center=input$center,
      center = 3,
      ordered = input$ordered
      #include.levels=input$include.levels
    )
    xtab
  },
  include.rownames = FALSE)
  
  #add ,caption.placement='top',include.rownames=FALSE
  output$plot <- renderPlot
  ({
    dataset <- datasetInput()
    {
      p <- plot(
        dataset,
        centered = input$centered,
        ordered = input$ordered,
        #center=input$center,
        center = 3,
        main = "Survey Data"
      )
      print(p)
    }
  })
  
  
})
