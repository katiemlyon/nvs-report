# Define UI for application
shinyUI(fluidPage(
 
  # Application title
  headerPanel("Survey Results"),
  
  # Sidebar
  sidebarPanel(#TODO read in defaults rather than setting
    selectInput(inputId = "dataset", 
                label = "Choose a Group of Questions",
                choices=c("Safe and Welcoming"),
                selected = NULL),
    checkboxInput("include.center", "Include center", TRUE),
    checkboxInput("centered", "Centered", TRUE),
    checkboxInput("ordered", "Ordered", TRUE),
    #numericInput("center", "Center", 4, step=0.5),
    #numericInput("wrap", "Length of wrapped text", 30),
    #textInput('caption','Table Caption:','This is a table.'),
    #may have to wrap xtable for reactivity here
    checkboxInput('include.n','Include n',TRUE),
    checkboxInput('include.mean','Include mean',TRUE),
    checkboxInput('include.sd','Include sd',TRUE),
    checkboxInput('include.low','Include disagree',TRUE),
    checkboxInput('include.neutral','Include neutral',TRUE),
    checkboxInput('include.high','Include agree',TRUE),
    checkboxInput('include.missing','Include missing',TRUE)
    
    #add next two lines to save plat
    #textInput('filename', "Filename"),
    #checkboxInput('savePlot', "Check to save"),
    
    ),
  

  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")), 
      #tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Percent", tableOutput('percent')),
      tabPanel("Statistics", tableOutput('stats'))
      )
  )
))
