

require(grid);require(colorspace);require(gridExtra)

#plot a single stamp
PlotStamp<-function(stampVal=30,stampWidth=.8,stampHeight=.8,outerColor="#3A0CA3",innerColorAlpha=0.2,borderWidth=0.2,fontSize=30,plot=F,nScallops=20){
nScallops=nScallops
nudgeEdge=0
#Make lighter innercolor based on alpha value that is a lighter, but opaque version
#of the outercolor (by mixing with white)
innerColor=colorspace::hex(colorspace::mixcolor(1-innerColorAlpha,colorspace::hex2RGB(outerColor),colorspace::RGB(1,1,1)))
#define radius of tiny circles that make the scalloped edge on stamp
scallop_radius=(1/nScallops)/2
  x1=scallop_radius
  x=x1+((1:nScallops-1)*(scallop_radius*2))
  y=scallop_radius+((1:nScallops-1)*(scallop_radius*2))
scallopCoords<-data.frame(x=c(x,x,rep(0,length(x)),rep(1,length(x))),
                          y=c(rep(0,length(x)),rep(1,length(x)),y,y),
                          r=scallop_radius)  
vp_stamp<-grid::viewport(width=stampWidth,height=stampHeight,clip="on")

grid::grid.newpage()
# grid::grid.draw(
  stamp_grob <- grid::gTree(children=grid::gList(
    grid::rectGrob(name = "outerRec",
                        x = 0.5, y = 0.5, width=stampWidth,
                        height=stampHeight,
                        gp = grid::gpar(col = "white",fill=outerColor),
                        vp=grid::viewport(x = 0.5, y = 0.5, width=1,
                        height=1,clip="on")),
    grid::rectGrob(name = "innerRec",
                        x = 0.5, y = 0.5, width=stampWidth*(1-borderWidth),
                        height=stampWidth*(1-borderWidth),
                        gp = grid::gpar(col = "gray30",fill=innerColor)),
  grid::circleGrob(name = "scallops",
    x = scallopCoords$x,
    y = scallopCoords$y,
    r = scallopCoords$r,
    gp = grid::gpar(fill = "white", col = "gray30"),
    vp = vp_stamp
    ),
  grid::grid.clip(x = 0.5, y = 0.5, width=stampWidth,
                        height=stampHeight),
  grid::textGrob(label = stampVal,x=0.5,y=0.5,gp = grid::gpar(fontsize=fontSize,
                                                             fontfamily="serif",
                                                             col="gray20"))
))

if(plot){grid::grid.draw(stamp_grob)}
return(stamp_grob)

}

# PlotStamp(plot=T)

#add more palettes (coolors.co is a great source)
palettes<-list(
  vaporwave=c("#F72585","#7209B7","#3A0CA3","#4361EE","#4CC9F0",
              "#f9c816","#52489C","#4062BB","#59C3C3","#F45B69"),
  postal=c("#ddd8b8ff", "#b3cbb9ff", "#84a9c0ff", "#6a66a3ff", "#542e71ff")
)

StyleStamps<-function(denominations,pal="postal",smallestStamp=0.75){
  # browser()
 pal=palettes[[match(pal,names(palettes))]]
   #set scalar for sizing denominations
  #ratio of smallest to largest size
  smallest=smallestStamp
  sVals<-sort(as.numeric(unique(denominations)),decreasing=T)
  #repeat the palette if necessary to match the length of the 
  #number of unique stamp values
  pal_final<-rep_len(pal,length.out = length(sVals))
   # browser()
   scales::show_col(pal_final)
  sizeDiffs=(1-smallest)/(length(sVals)-1)
  size_factors<-c(1,rep(NA,length(sVals)-1))
  # browser()
  #make size gradations between 1 and smallest if more than 1 denomination
  if(length(denominations)>1){
    for(ii in 2:length(sVals)){
      size_factors[ii]<-size_factors[[ii-1]]-sizeDiffs
      }
  }
  return(data.frame(stampVal=sVals,colors=pal_final,size_factors=size_factors))
  }



#Iterate across all stamp_data
MakeStampPNG<-function(denominations,borderWidth,nScallops,pal="postal",...){
  maxCol=5 #maximum number of stamps per row
  
  #get color and size info for stamp denominations
  stamp_styles<-StyleStamps(denominations,pal=pal,...)
  
  stamp_batch<-lapply(1:length(denominations),function(i){
      #Set largest stampWidth
      defaultWidth=0.99
      #get relative width, scaled by stamp value
      styleRow<-which(stamp_styles$stampVal==denominations[i])
      rel_width<-defaultWidth*stamp_styles$size_factors[styleRow]
      #Actually plot a stamp
      PlotStamp(denominations[i],stampWidth =defaultWidth,stampHeight=defaultWidth,
                outerColor=stamp_styles$colors[styleRow],plot=F,nScallops=nScallops,
                borderWidth=borderWidth)
  })
  # batch1<-do.call(grid::gList,stamp_batch[[1]])  
  # cowplot::plot_grid(batch1[[1]],batch1[[2]],ncol=2)
  # gridExtra::grid.arrange(batch1,ncol=2)
  names(stamp_batch)<-denominations
  list(plots=stamp_batch,styles=stamp_styles)
}

# stamp_data<-list(data.frame(startVal=c(13,10),stampVal=c(13,10),divisor=c(10,13),remaining=c(0,0)))
# stamps<-Stampify(stamp_data,100)
# grid.draw(stamps[[2]])
# 


# The big kahuna ----------------------------------------------------------

AddPostage<- function(calculator_result,label,stamp_styles, base_stamp_sz=50,stamps_per_row=5){
     #testing repeating an image object
     solns<-unique(calculator_result$startVal)
     tagList(
         h3(class="combo-heading",label),
         lapply(solns,function(soln_i){
           #create a solution div for each unique solution
           curr_soln<-subset(calculator_result,startVal==soln_i)
           div(class="solution",
        
              #Plot Stamp images
              #in-line styling to set the number of stamps per row; additional styling can be done in custom.css
              div(class="stamp-container",style=paste0("width:",
                                                       #an awkward solution to keep stamps wrapping as expected
                                                       (stamps_per_row*base_stamp_sz)-10,
                                                           "px;"),
                 tagList(
                   lapply(1:nrow(curr_soln),function(i){
                     curr_stamp<-curr_soln[i,]
                     curr_stamp_sz<-stamp_styles$size_factors[which(stamp_styles$stampVal==curr_stamp$stampVal)]
                     #repeat images the desired amount
                     lapply(1:curr_stamp$stampN,function(ii){
                       img(class="stamp",src=paste0("temp/",curr_stamp$stampVal,".png"),
                           width=curr_stamp_sz*base_stamp_sz,height=curr_stamp_sz*base_stamp_sz)
                     })
                 }))),
              #plot text solution
                p(class="solution-text",
                   paste0(c(sapply(1:nrow(curr_soln),function(row_i){
                     paste0(curr_soln$stampN[row_i]," x ",curr_soln$stampVal[row_i],"c stamps")
                   }),paste0(abs(tail(curr_soln,1)$remaining),"c over")),
                   collapse=", "
                   
                    ))
              )#End solution div
         })
       )
}
