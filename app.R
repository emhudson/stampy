#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny);library(magrittr);library(shinythemes)


source("StampCalculator.R")
source("PlotStamps.R")
#Normal r stuff can be put here, outside ui
#can read in global vars to use in any part of ui

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
    #import custom styling
     tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
    # Application title
    titlePanel("Stampy: a tool to show postage stamp combinations"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            selectizeInput("stampValues","Available Stamp Values",choices=c(3,10,13,20,32,58),selected=list(3,10,13,20,32,58),
                           multiple=T,
                           options=list(create=T,placeholder="Stamp values")),
            selectizeInput("totalfare","Total Fare",choices=130,
                           multiple=F,
                           options=list(create=T,placeholder="Total fare")),
            selectizeInput("maxStamps","Maximum number of one kind of stamp?",choices=10,
                           multiple=F,
                           options=list(create=T,placeholder="Maximum stamp repeats")),
            actionButton("calculate","Calculate")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           # textOutput("consoleTxt"),
           # textOutput("result1"),
           # p(strong("Combo for exact fare:")),
           # textOutput("Exact"),
           # p(strong("Combo for fewest stamps:")),
           # textOutput("Fewest"),
           # p(strong("Fewest stamps with least overage")),
           # textOutput("Score"),
           #Output main stamp plots
           uiOutput("main")
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

    
  observe({
      #R code
    
      orderedstamps<-sort(as.numeric(input$stampValues), decreasing = T)
      output$result1 <- renderText(orderedstamps)
      
      combos <- makeStampCombos(vals = orderedstamps,totalfare = as.numeric(input$totalfare))
      #browser()
      if (length(combos)==1){
        output$main <- renderUI(h4(combos,class="error"))
      } else {
      maxNum <- as.numeric(input$maxStamps)
   
      combos$exact <- subset(combos$exact, combos$exact$startVal %in% subset(combos$summary,combos$summary$stampN <= maxNum)$startVal) #no exact solutions with over 10 stamps of one kind
      combos$minscore <- subset(combos$minscore, combos$minscore$startVal %in% subset(combos$summary,combos$summary$stampN <= maxNum)$startVal)
      #Print exact combo
      #output$Exact <- renderPrint(c(paste(combos$exact$stampN," x ",combos$exact$stampVal,"cent stamps,"),paste(combos$exact$remaining[nrow(combos$exact)],"cents over" )))
      #Print fewest stamp combo
     # output$Fewest <- renderPrint(c(paste(combos$fewest$stampN," x ",combos$fewest$stampVal,"cent stamps,"),paste(combos$fewest$remaining[nrow(combos$exact)],"cents over" )))
 
      #Print Lowest score
      #output$Score <- renderPrint(c(paste(combos$minscore[1:nrow(combos$minscore),]$stampN," x ",combos$minscore[1:nrow(combos$minscore),]$stampVal,"cent stamps,"), paste(-combos$minscore$remaining[nrow(combos$minscore)],"cents over"))) #minscore not working
      
  
        # Generate Stamp Images -------------------------------------------------------------
      
        stamps<-MakeStampPNG(input$stampValues,borderWidth=0.4,nScallops=11,pal="postal")
        #where we gonna save stamp images temporarily?
        img_loc<-paste0(getwd(),"/www/temp/")
        #make that dir if it doesn't exist
        dir.create(img_loc,showWarnings=F)
        #delete and rewrite all temp files
        unlink(list.files(img_loc,pattern=".png"))
        #now save those stamp plots as images
         lapply(1:length(stamps$plots),function(i) {
           png(paste0(img_loc,names(stamps$plots)[i],".png"),width=200,height=200, units="px",res=150)
           grid.draw(stamps$plots[[i]])
           dev.off()
          })  
          
      # Plot Stamps -------------------------------------------------------------
         
           
         output$main<-renderUI({list(AddPostage(combos$exact,label="Exact solution(s):",stamp_styles   =stamps$styles),
      AddPostage(combos$fewest,"Fewest stamps:",stamp_styles=stamps$styles),
      AddPostage(combos$minscore,"Fewest stamps with least overpayment:",stamp_styles=stamps$styles),
     # browser(),
      if (orderedstamps[1]> as.numeric(input$totalfare)){
        AddPostage(combos$onestamp, "One stamp solution:", stamp_styles=stamps$styles)
      },
      #add spacer at bottom of page
      div(class="spacer")
     
  )
    })#end renderUI
}
}) %>% bindEvent(input$calculate)#End observe 
}# End server logic
    



# Run the application 
shinyApp(ui = ui, server = server)
