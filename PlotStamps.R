

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
  vaporwave=c("#F72585","#7209B7","#3A0CA3","#4361EE","#4CC9F0")
)

StyleStamps<-function(denominations,pal=palettes[[1]]){
  #set scalar for sizing denominations
  #ratio of smallest to largest size
  smallest=0.5
  sVals<-sort(as.numeric(unique(denominations)),decreasing=T)
  #repeat the palette if necessary to match the length of the 
  #number of unique stamp values
  pal_final<-rep_len(pal,length.out = length(sVals))
  # browser()
  sizeDiffs=smallest/(length(sVals)-1)
  size_factors<-c(1,rep(NA,length(sVals)-1))
  for(ii in 2:length(sVals)){size_factors[ii]<-size_factors[[ii-1]]-sizeDiffs}
  return(data.frame(stampVal=sVals,colors=pal_final,size_factors=size_factors))
  }



#Iterate across all stamp_data
PlotMultStamps<-function(denominations,borderWidth,nScallops,...){
  maxCol=5 #maximum number of stamps per row
  
  #get color and size info for stamp denominations
  stamp_styles<-StyleStamps(denominations,...)
  
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
