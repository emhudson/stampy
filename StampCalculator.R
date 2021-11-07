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
totalfare <- 130
vals <- c(58,37,32,20,13,3)
# vals <- c(58,37,20,13)
## Matt approach
#Loop for starting value
makeStampCombos <- function(vals, totalfare){
OUT.0<-lapply(1:length(vals),function(i){
  print(paste("loop through",i,"cur stamp is ",vals[i]))
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
    
    print(paste("j=",j,"cur next stamp is",vals[j]))
    if (is.na(vals[j])){
      print("uh oh, cur stamp is NA")
      print(paste("and cur StampN is",stampN))
      remaining <- remaining - vals[j-1]
      stampN <- stampN +1
      print(paste("Did we fix it? new remaining =",remaining, "new stampN = ",stampN))
      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j-1],
                          stampN=stampN,
                          remaining=remaining)
      print("new row version X, final final")
      print(new_row)
      out<-new_row
      break()
    }
    if (j>=length(vals)) { #special case for last row: check if any remainder left
      stampN<-remaining%/%vals[j-1]
      remaining <-remaining%%vals[j-1]
      print(paste("last row, stampN =",stampN,"rem =",remaining))
      while (remaining>0){
        print("remaining greater than zero in the last row")
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
    
      out<-rbind(out,new_row)
     
    }#j loop end
 
  out
  #bind all previous
}#function end
)
names(OUT.0)<-vals

#end lapply
require(dplyr)
OUT <- OUT.0 %>% bind_rows()

#bigstampN= stampN[stampN$startVal==stampN$stampVal]$stampVal
bigstampN <- OUT %>% group_by(startVal) %>% filter(row_number()==1) %>% mutate(comboname = paste0(stampN,"x",startVal)) %>% select(c(startVal,comboname))


stampN <- OUT %>% group_by(startVal) %>% summarise(stampN=sum(stampN))

overPymt<-OUT %>% group_by(startVal) %>% summarise(overPymt=min(remaining))

summaryStamps <- left_join(stampN,overPymt)
summaryStamps <- left_join(bigstampN,summaryStamps)
summaryStamps$score <- summaryStamps$stampN + abs(summaryStamps$overPymt)

exact = subset(OUT, OUT$startVal==summaryStamps[(summaryStamps$overPymt == 0),]$startVal)
exact = subset(exact, exact$stampN!=0)

exactcombo <- paste(exact$stampN," x ",exact$stampVal,"cent stamps,",exact$remaining,"cents over" )


minimize_num <- subset(OUT,OUT$startVal==summaryStamps[which.min(summaryStamps$stampN),][1,]$startVal)
minimize_num = minimize_num[minimize_num$stampN!=0,]
mininum_combo <- paste(minimize_num[1,2:3]$stampN," x ",minimize_num[1,2:3]$stampVal,"cent stamps,",minimize_num[2,2:3]$stampN," x ",minimize_num[2,2:3]$stampVal,"cent stamps,",-minimize_num[2,2:4]$remaining,"cents over")


minimize_score <- subset(OUT, OUT$startVal==summaryStamps[summaryStamps$score==min(summaryStamps$score),][2,]$startVal)
minimize_score <- minimize_score[minimize_score$stampN!=0,]
miniscore_combo <- paste(minimize_score[1,2:3]$stampN," x ",minimize_score[1,2:3]$stampVal,"cent stamps,",minimize_score[2,2:3]$stampN," x ",minimize_score[2,2:3]$stampVal,"cent stamps,",-minimize_score[2,3:4]$remaining,"cents over")



#list(Exact = exactcombo, Fewest = mininum_combo, Score = miniscore_combo, summaryStamps)
list(data=OUT.0,exact=exact,fewest=minimize_num,minscore=minimize_score)
#browser()
}


