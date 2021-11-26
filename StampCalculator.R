#Pseudocode:

# #Take inventory of values
# #Get target total fare
# stampa <- 58
# stampb <- 37
# stampc <- 20
# stampd <- 13
# 
# # passes <- 9
# # passa <- 0 
# # passb <- 0
# # passc <- 0
# # passd <- 0
# # 
# # combinations <-matrix(ncol=2, nrow = 1000)
# # 
# # while (passa <= passes) {
# #   keya <- passa
# #   valuea <- stampa * passa #value of 
# #   passa <- passa + 1
# #   passb <- 0 # reset passb
# #   while (passb <= passes) {
# #     keyb <- passb
# #     valueb <- stampb * passb
# #     passb <- passb + 1
# #     passc <- 0 # reset passc
# #     while (passc <= passes) {
# #       keyc <- passc
# #       valuec <- stampc * passc
# #       passc <- passc + 1
# #       passd <- 0 # reset passd
# #       while (passd <= passes) {
# #         keyd <- passd
# #         valued <- stampd * passd
# #         key <- c('', keya,keyb,keyc, keyd)
# #         value <-c(valuea , valueb , valuec , valued)
# #         combinations <- append(combinations, key, value)#{name:key, value:value})
# #         passd <- passd + 1
# #       }
# #     }
# #   }
# # }
# # 
# # head(combinations,10)
# 
# ### Smallest number of stamps solution
# 
# #Divide by largest denomination, remainder integer is new target
# #will this always give fewest number of stamps?
# 
# #also check if second-largest is exact factor
# 
# ###Smallest number of stamps with small overage
# 
# ### Most unique values of stamps
# 
# ### "Must include" values
# 
# 
# #target fare divided by maximum value
# 
# #Two things stored: remaining fare, remaining slots
# 
# 
#totalfare <- 130
#vals <- c(58,37,32,20,13,3)
# vals <- c(58,37,20,13)
## Matt approach
#Loop for starting value
makeStampCombos <- function(vals, totalfare){
  #browser()
  
  #Input error states: OUT.0 is just an error message
  if (identical(totalfare, as.integer(0)) |
  identical(totalfare, NULL) |
  identical(totalfare,NA) |
  is.na(totalfare)|
  identical(length(totalfare),as.integer(0))) {
  OUT.0<- "Please enter a total fare (postage rate) value"
  return(OUT.0)
  
} else if (identical(vals, 0) |
           identical(vals, NULL) |
           identical(vals,NA) |
           identical(length(vals),as.integer(0))) {
  OUT.0 <- "Please enter at least one stamp value"
  return(OUT.0)
 
  #Stamp calculating code begins
} else {
  OUT.0<-lapply(1:length(vals),function(i){

  print(paste("loop through",i,"starting stamp is ",vals[i]))
  remaining <- totalfare%%vals[i]
  stampN <- totalfare%/%vals[i]
  out<-data.frame(startVal=vals[i],
                  stampVal=vals[i],
                  stampN=totalfare%/%vals[i],
                  remaining=remaining
                  )
  print(out)
  #Loop iterating remaining values

  for (j in (i+1):length(vals)){ 
    print(paste(vals[i],"combo," ,"j=",j,"cur next stamp is",vals[j]))
    #Last stamp in the input, special case to handle NA stamp; redundant with if below?
    if (is.na(vals[j])){
      print("cur stamp is NA -- must be at the end")
      print(paste("and fare remaining is",remaining))

      # browser()
      # remaining <- remaining - vals[j-1]
      # stampN <- stampN +1
      if (remaining > 0){
        stampN <- stampN +1
        remaining <- remaining - vals[j-1]
        print(paste("new remaining =",remaining, "new stampN = ",stampN))


      }
      else {
      print("no remainder, we're done")

      }

      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j-1],
                          stampN=stampN,
                          remaining=remaining)
      print("finished last stamp val")
      print("new row:")
      print(new_row)
      #out<-rbind(out,new_row)
      out <- new_row
      print(out)
      break()
    }
    
    if (j>=length(vals)) { #special case for last row: check if any remainder left
      stampN<-remaining%/%vals[j-1]
      remaining <-remaining%%vals[j-1]
      print(paste("last row of this startval, stampN =",stampN,"rem =",remaining))
      while (remaining>0){
        print("remaining greater than zero for last row")
        if (is.na(vals[j])){
          remaining <- remaining - vals[j-1]
          stampN <- stampN +1
          print(paste("new remaining =",remaining, "new stampN = ",stampN))
        } else {
        #print(paste(remaining,"-",vals[j],"j = (",j,")"))
        remaining <- remaining - vals[j]
        stampN <- stampN +1
        print(paste("new remaining =",remaining, "new stampN = ",stampN))
        }
      }
      #A 
     
      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j],
                          stampN=stampN,
                          remaining=remaining)
      print("new row version A, final")
      print(new_row)
    } else if (remaining%/%vals[j]>0){ #If this stamp value goes into remainder, record how many times (stampN)
     print(paste("Stamp value",vals[j],"goes into",remaining))
      stampN<-remaining%/%vals[j]
      remaining <-remaining%%vals[j]
      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j],
                          stampN=stampN,
                          remaining=remaining)
      print("new row version B")
      print(new_row)
     }
    else {
      print(paste("Stamp val", vals[j],"does not go into",remaining))
      stampN <- remaining%/%vals[j] #should be zero
      remaining <-remaining%%vals[j]
      new_row<-data.frame(startVal=vals[i],
                      stampVal=vals[j],
                      stampN=stampN,
                      remaining=remaining)
      print("new row version c, none of this stamp")
      print(new_row)
                      
    }
    
    #browser()  
    out<-rbind(out,new_row)
    #browser()
    }#end of current startVal, 
 
  out #?? This is the current stamp start val combo. For 3 stamp problem, this has wrong stampN (3 rather than 4)
  #browser()
  
  #bind all previous
}#lapply end
)
#browser()
#out #doesn't exist here
OUT.0
names(OUT.0)<-vals

#browser()
require(dplyr)
OUT <- OUT.0 %>% bind_rows() #add this round of stamps to the total outup
}


#browser()
#bigstampN= stampN[stampN$startVal==stampN$stampVal]$stampVal

bigstampN <- OUT %>% group_by(startVal) %>% filter(row_number()==1) %>% mutate(comboname = paste0(stampN,"x",startVal)) %>% dplyr::select(c(startVal,comboname))


stampN <- OUT %>% group_by(startVal) %>% summarise(stampN=sum(stampN))

overPymt<-OUT %>% group_by(startVal) %>% summarise(overPymt=min(remaining))

summaryStamps <- left_join(stampN,overPymt)
summaryStamps <- left_join(bigstampN,summaryStamps)
summaryStamps$score <- summaryStamps$stampN + abs(summaryStamps$overPymt)

exact = subset(OUT, OUT$startVal %in% summaryStamps[(summaryStamps$overPymt == 0),]$startVal)
  
  #subset(OUT, OUT$startVal==summaryStamps[(summaryStamps$overPymt == 0),]$startVal)
exact = subset(exact, exact$stampN!=0)


exact_printout <- c(paste(exact$stampN," x ",exact$stampVal,"cent stamps,"),paste(exact$remaining[nrow(exact)],"cents over" ))


minimize_num <- subset(OUT,OUT$startVal==summaryStamps[which.min(summaryStamps$stampN),][1,]$startVal)
minimize_num = minimize_num[minimize_num$stampN!=0,]
mininum_combo <- paste(minimize_num[1,2:3]$stampN," x ",minimize_num[1,2:3]$stampVal,"cent stamps,",minimize_num[2,2:3]$stampN," x ",minimize_num[2,2:3]$stampVal,"cent stamps,",-minimize_num[2,2:4]$remaining,"cents over")


minimize_score <- subset(OUT, OUT$startVal %in% summaryStamps[summaryStamps$score==min(summaryStamps$score),]$startVal)
#minimize_score <- OUT[OUT$startVal==summaryStamps[summaryStamps$score==min(summaryStamps$score),]$startVal,]


minimize_score <- minimize_score[minimize_score$stampN!=0,]
miniscore_combo <- paste(minimize_score[1,2:3]$stampN," x ",minimize_score[1,2:3]$stampVal,"cent stamps,",minimize_score[2,2:3]$stampN," x ",minimize_score[2,2:3]$stampVal,"cent stamps,",-minimize_score[2,3:4]$remaining,"cents over")

if (vals[1]>totalfare){
  onestamp_val <-tail(vals[which(vals > totalfare)],1)
  onestamp <- data.frame(onestamp_val,onestamp_val,1,totalfare-onestamp_val)
  names(onestamp) <- names(minimize_score)
  #also, you'll need to remove duplicate combos
  #take only last two rows of exact, minimize_score
  #take only rows where startVal <= totalfare
  exact <- subset(exact, exact$startVal <= totalfare)
  minimize_score <- subset(minimize_score, minimize_score$startVal <= totalfare)
} else {
  onestamp <- NULL
}

#list(Exact = exactcombo, Fewest = mininum_combo, Score = miniscore_combo, summaryStamps)
finalout <- list(data=OUT.0,summary=summaryStamps,exact=exact,fewest=minimize_num,minscore=minimize_score,onestamp=onestamp)

#browser()
} #function end



