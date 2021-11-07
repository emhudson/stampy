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
OUT<-lapply(1:length(vals),function(i){
  remaining <- totalfare%%vals[i]
  out<-data.frame(startVal=vals[i],
                  stampVal=vals[i],
                  divisor=totalfare%/%vals[i],
                  remaining=remaining
                  )

  #Loop iterating remaining values
  for (j in 2:length(vals)){ 
    if (j==length(vals)) { #special case for last row: check if any remainder left
      
      divisor<-remaining%/%vals[j]
      remaining <-remaining%%vals[j]
      if (remaining>0){
        remaining <- remaining - vals[j]
        divisor <- divisor +1
      }
      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j],
                          divisor=divisor,
                          remaining=remaining)
    } else if (remaining%/%vals[j]>0){ #If this stamp value goes into remainder, record how many times (divisor)
      divisor<-remaining%/%vals[j]
      remaining <-remaining%%vals[j]
      new_row<-data.frame(startVal=vals[i],
                          stampVal=vals[j],
                          divisor=divisor,
                          remaining=remaining)
     }
    else {
      divisor <- remaining%/%vals[j] #should be zero
      remaining <-remaining%%vals[j]
      new_row<-data.frame(startVal=vals[i],
                      stampVal=vals[j],
                      divisor=divisor,
                      remaining=remaining)
                      
    }
    
      out<-rbind(out,new_row)
      
    }#j loop end
  out
  #bind all previous
}#function end
)
#end lapply
require(dplyr)
OUT <- OUT %>% bind_rows()

stampN <- OUT %>% group_by(startVal) %>% summarise(stampN=sum(divisor))

overPymt<-OUT %>% group_by(startVal) %>% summarise(overPymt=min(remaining))

summaryStamps <- left_join(stampN,overPymt)

summaryStamps$score <- summaryStamps$stampN + abs(summaryStamps$overPymt)

exact = subset(OUT, OUT$startVal==summaryStamps[(summaryStamps$overPymt == 0),]$startVal)
exact = subset(exact, exact$divisor!=0)

exactcombo <- paste(exact$divisor," x ",exact$stampVal,"cent stamps,",exact$remaining,"cents over" )
exact[,2:3]

minimize_num <- subset(OUT,OUT$startVal==summaryStamps[which.min(summaryStamps$stampN),][1,]$startVal)
minimize_num = minimize_num[minimize_num$divisor!=0,]
mininum_combo <- paste(minimize_num[1,2:3]$divisor," x ",minimize_num[1,2:3]$stampVal,"cent stamps,",minimize_num[2,2:3]$divisor," x ",minimize_num[2,2:3]$stampVal,"cent stamps,",-minimize_num[2,2:4]$remaining,"cents over")
minimize_num[,2:3]

minimize_score <- subset(OUT, OUT$startVal==summaryStamps[summaryStamps$score==min(summaryStamps$score),][2,]$startVal)
minimize_score <- minimize_score[minimize_score$divisor!=0,]
miniscore_combo <- paste(minimize_score[1,2:3]$divisor," x ",minimize_score[1,2:3]$stampVal,"cent stamps,",minimize_score[2,2:3]$divisor," x ",minimize_score[2,2:3]$stampVal,"cent stamps,",-minimize_score[2,3:4]$remaining,"cents over")



list(Exact = exactcombo, Fewest = mininum_combo, Score = miniscore_combo, summaryStamps)
#browser()
}


