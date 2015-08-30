rm(list = ls())
load("straightStart.Rdata")

weekFileOrig <- read.csv("2015week01straight.csv", stringsAsFactors = F)
weekFileOrig <- cbind(order = 1:16, weekFileOrig)
weekFile <- weekFileOrig[order(-weekFileOrig$WinProbability), ]
winprob <- weekFile$WinProbability[order(-weekFile$WinProbability)]
outcomeMatrix <- matrix(runif(16 * 1700) < winprob, nrow = 16)
# outcomeMatrix[, 1:10]

comparisonPicksScores <- crossprod(outcomeMatrix, comparisonPicks) + crossprod((1- outcomeMatrix), (1 - comparisonPicks))

fanprob <- weekFile$FanProb[order(-weekFile$WinProbability)]
fanMatrix <- matrix((fanMatrix < fanprob) * 1, nrow = 16)
fanScores <- crossprod(outcomeMatrix, fanMatrix) + crossprod((1- outcomeMatrix), (1 - fanMatrix))
fanSubset <- matrix(rep(0, 1700 * 250), nrow = 1700)
sampleFans <- matrix(sample(1:1700, 1700 * 250, replace = T), nrow = 1700)

for (i in 1:1700){
  fanSubset[i, ] <- fanScores[i, sampleFans[i, ]]
}
rm(fanScores)


save.image("2015wk01.RData")
###
load("2015wk01.RData")

# suppressMessages(library(foreach))
# suppressMessages(suppressWarnings(library(data.table)))
poolsize <- seq(5, 50, by = 5)
outright <- rep(1, 10)
mostwins <- rep(1, 10)

calcTactics <- function(size){#size=20
  fanScoreSubset <- fanSubset[, 1:size]
  
  comparisonFirst <- comparisonPicksScores > apply(fanScoreSubset, 1, max)
  comparisonTiedorFirst <- comparisonPicksScores >= apply(fanScoreSubset, 1, max)
  
  fansFirst <- 1 * (fanScoreSubset == apply(fanScoreSubset, 1, max))
  fansTiedorFirstCount <- rowSums(fansFirst)
  fansTiedorFirstAvg <- mean(fansTiedorFirstCount)/size*17
  fansFirstCount <- rep(0, 1700)
  fansFirstCount[fansTiedorFirstCount == 1] <- 1
  fansFirstAvg <- mean(fansFirstCount)/size*17
  
  outright <- which(colSums(comparisonFirst) == max(colSums(comparisonFirst)))
  lenOut <- length(outright)
  mostwins <- which(colSums(comparisonTiedorFirst/fansTiedorFirstCount) == max(colSums(comparisonTiedorFirst/fansTiedorFirstCount)))
  lenMost <- length(mostwins)
  
  data <- list(outright, mostwins, numOutright = lenOut, numWins = lenMost, outPicks = comparisonPicks[, outright], mostPicks = comparisonPicks[, mostwins], outW = colSums(comparisonFirst)[outright]/100, mostW = colSums(comparisonTiedorFirst)[mostwins]/100, avgOut = fansFirstAvg, avgMost = fansTiedorFirstAvg)
  data
}
# library(compiler)
# cmpCalcTac <- cmpfun(calcTactics)
popList <- function(size){list(size, calcTactics(size))}

system.time(firstList <- popList(5))
playersBest <- rep(firstList, 50)

# suppressMessages(suppressWarnings(library(data.table)))

fanSizes <- seq(5, 100, by = 5)

compTactics <- function(){
  for(i in 2:20)  {#i = 2
    
    size <- fanSizes[i]
    genList <- popList(size)
    playersBest[[2*(i - 1) + 1]] <<- genList[[1]]
    playersBest[[2*i]] <<- genList[[2]]
    
  }
}

system.time(compTactics())

weekFile
weekFileOrig

save(weekFile, weekFileOrig, playersBest, file = "app2015wk01.RData")

