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
source("PlotStamps.R")
#Normal r stuff can be put here, outside ui
#can read in global vars to use in any part of ui

# Define UI for application that draws a histogram
ui <- fluidPage(
    #import custom styling
     tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      ),
    # Application title
    titlePanel("Stampy: a calculator to figure out what stamps to add to your postcards and such"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           
            selectizeInput("stampValues","Stamp Values",choices=c(10,20,30),selected=list(10,20,30),
                           multiple=T,
                           options=list(create=T,placeholder="Stamp values")),
            selectizeInput("totalfare","Total Fare",choices=60,selected=list(60),
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
           textOutput("Score"),
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
      #browser()
      combos <- makeStampCombos(vals = orderedstamps,totalfare = as.numeric(input$totalfare))
      #Print exact combo
      output$Exact <- renderPrint(c(paste(combos$exact$stampN," x ",combos$exact$stampVal,"cent stamps,"),paste(combos$exact$remaining[nrow(combos$exact)],"cents over" )))
      #Print fewest stamp combo
      output$Fewest <- renderPrint(c(paste(combos$fewest[1,2:nrow(combos$fewest)]$stampN," x ",combos$fewest[1,2:nrow(combos$fewest)]$stampVal,"cent stamps,",combos$fewest[2,2:nrow(combos$fewest)]$stampN," x ",combos$fewest[2,2:nrow(combos$fewest)]$stampVal,"cent stamps,"),paste(-combos$fewest$remaining[nrow(combos$fewest)],"cents over")))
      #Print Lowest score
      output$Score <- renderPrint(c(paste(combos$minscore[1:nrow(combos$minscore),]$stampN," x ",combos$minscore[1:nrow(combos$minscore),]$stampVal,"cent stamps,"), paste(-combos$minscore$remaining[nrow(combos$minscore)],"cents over")))
  
        # Generate Stamp Images -------------------------------------------------------------
      
        stamps<-PlotMultStamps(input$stampValues,borderWidth=0.4,nScallops=11)
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
  output$main<-renderUI({
     #base png output size in px
     base_stamp_sz=50
     #max number of stamps you want per row
     stamps_per_row=5
     
     #testing repeating an image object
     solns<-unique(combos$exact$startVal)
     tagList(
         h3(class="combo-heading","Exact solution(s):"),
         lapply(solns,function(soln_i){
           #create a solution div for each unique solution
           curr_soln<-subset(combos$exact,startVal==soln_i)
           div(class="solution",
               # browser(),
               p(class="solution-text",
                   paste0(c(sapply(1:nrow(curr_soln),function(row_i){
                     paste0(curr_soln$stampN[row_i]," x ",curr_soln$stampVal[row_i],"c")
                   }),paste0(tail(curr_soln,1)$remaining," remaining")),
                   collapse=", "
                   )
               ),
              #in-line styling to set the number of stamps per row; additional styling can be done in custom.css
             div(class="stamp-container",style=paste0("width:",stamps_per_row*base_stamp_sz,"px;"),
             tagList(
               lapply(1:nrow(curr_soln),function(i){
                 curr_stamp<-curr_soln[i,]
                 # browser()
                 curr_stamp_sz<-stamps$styles$size_factors[which(stamps$styles$stampVal==curr_stamp$stampVal)]
                 #repeat images the desired amount
                 lapply(1:curr_stamp$stampN,function(ii){
                   img(class="stamp",src=paste0("temp/",curr_stamp$stampVal,".png"),width=curr_stamp_sz*base_stamp_sz,height=curr_stamp_sz*base_stamp_sz)
                 })
             }))
             )
           )
         })
       )
    })#end renderUI

}) %>% bindEvent(input$calculate)#End observe 
}# End server logic
    



# Run the application 
shinyApp(ui = ui, server = server)
