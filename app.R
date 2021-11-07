#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(magrittr)

source("StampCalculator.R")
#Normal r stuff can be put here, outside ui
#can read in global vars to use in any part of ui

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Stampy: a calculator to figure out what stamps to add to your postcards and such"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            selectizeInput("stampValues","Stamp Values",choices=NULL,
                           multiple=T,
                           options=list(create=T,placeholder="Stamp values")),
            selectizeInput("totalfare","Total Fare",choices=NULL,
                           multiple=F,
                           options=list(create=T,placeholder="Total fare")),
            actionButton("calculate","Calculate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("consoleTxt"),
           textOutput("result1"),
           p(strong("Combo for exact fare:")),
           textOutput("Exact"),
           p(strong("Combo for fewest stamps:")),
           textOutput("Fewest"),
           p(strong("Fewest stamps with least overage")),
           textOutput("Score")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({ #things that happen in renderPlot stay in renderPlot
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
    vals<-reactiveValues() #create reactive list, everything gets updated when accessed
    #observe({
    #  vals$inputStampVals<-sort(as.numeric(input$stampValues), decreasing = T)}) #
    
    output$consoleTxt<-renderPrint(input$stampValues)

    
    observeEvent(input$calculate,{
      #R code
      # source("Rfile.r")
      orderedstamps<-sort(as.numeric(input$stampValues), decreasing = T)
      output$result1 <- renderText(orderedstamps)
      # browser()
      combos <- makeStampCombos(vals = orderedstamps,totalfare = as.numeric(input$totalfare))
      output$Exact <- renderPrint(paste(combos$exact$stampN," x ",combos$exact$stampVal,"cent stamps,",combos$exact$remaining,"cents over" ))
      output$Fewest <- renderPrint(paste(combos$fewest[1,2:3]$stampN," x ",combos$fewest[1,2:3]$stampVal,"cent stamps,",combos$fewest[2,2:3]$stampN," x ",combos$fewest[2,2:3]$stampVal,"cent stamps,",-combos$fewest[2,2:4]$remaining,"cents over"))
      output$Score <- renderPrint(paste(combos$minscore[1,2:3]$stampN," x ",combos$minscore[1,2:3]$stampVal,"cent stamps,",combos$minscore[2,2:3]$stampN," x ",combos$minscore[2,2:3]$stampVal,"cent stamps,",-combos$minscore[2,3:4]$remaining,"cents over"))
    }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
