#test dataframe


#plot a stamp
plotStamp<-function(stampVal=30,stampWidth=.8,stampHeight=.8){
nScallops=15
nudgeEdge=0.009

scallop_radius=(1/nScallops)/2
  x1=scallop_radius
  x=x1+((1:nScallops-1)*(scallop_radius*2))
  y=scallop_radius+((1:nScallops-1)*(scallop_radius*2))
scallopCoords<-data.frame(x=c(x,x,rep(0,length(x)),rep(1,length(x))),
                          y=c(rep(0,length(x)),rep(1,length(x)),y,y),
                          r=scallop_radius)  
vp_stamp<-grid::viewport(width=stampWidth,height=stampHeight,clip="on")

scallops<-  grid::circleGrob(
    x = scallopCoords$x,
    y = scallopCoords$y,
    r = scallopCoords$r,
    gp = grid::gpar(fill = "black", col = "gray10"),
    vp = vp_stamp
)


grid::grid.newpage()
# grid::grid.draw(
  stamp_grob <- grid::gTree(children=grid::gList(
    grid::rectGrob(name = "outerRec",
                        x = 0.5, y = 0.5, width=stampWidth,
                        height=stampHeight,
                        gp = grid::gpar(col = "gray10",fill="gray")),
    grid::rectGrob(name = "innerRec",
                        x = 0.5, y = 0.5, width=.6,
                        height=.6,
                        gp = grid::gpar(col = "gray30",fill="gray80")),
  grid::circleGrob(name = "scallops",
    x = scallopCoords$x,
    y = scallopCoords$y,
    r = scallopCoords$r,
    gp = grid::gpar(fill = "white", col = "gray10"),
    vp = vp_stamp
    )
))
grid::grid.clip(x = 0.5, y = 0.5, width=stampWidth,
                        height=stampHeight)
grid::grid.draw(stamp_grob)
grid::grid.text(label = stampVal,x=0.5,y=0.5,gp = grid::gpar(fontsize=35))


}

plotStamp(40,0.6,0.6)




stamp_data<-data.frame(startVal=c(37,13),stampVal=c(3,13),divisor=c(2,10),remaining=c(0,0))
PlotStamps<-function(stamp_data,total_fare){
  lapply(1:nrow(stamp_data),function(i){
    maxCol=5 #maximum number of stamps per row
    d<-stamp_data[i,]
    stamp_grid<-
    ggplot2::ggplot(stamp_data,aes())
  
    
  })
}