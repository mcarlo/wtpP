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

poolsize <- 22
fanScoreSubset <- fanSubset[, 1:poolsize]

comparisonFirst <- comparisonPicksScores > apply(fanScoreSubset, 1, max)
comparisonTiedorFirst <- comparisonPicksScores >= apply(fanScoreSubset, 1, max)

outright <- which(colSums(comparisonFirst) == max(colSums(comparisonFirst)))
mostwins <- which(colSums(comparisonTiedorFirst) == max(colSums(comparisonTiedorFirst)))

# comparisonPicks[, outright]
# comparisonPicks[, mostwins]

colSums(comparisonFirst)[outright] +
  0.5 * (colSums(comparisonTiedorFirst)[outright] -
           colSums(comparisonFirst)[outright])
colSums(comparisonFirst)[mostwins] +
  0.5 * (colSums(comparisonTiedorFirst)[mostwins] -
           colSums(comparisonFirst)[mostwins])
sum(comparisonPicks[, mostwins] * winprob)
sum(comparisonPicks[, outright] * winprob)

colSums(comparisonTiedorFirst)[mostwins]
colSums(comparisonTiedorFirst)[outright]

colSums(comparisonFirst)[outright]
colSums(comparisonFirst)[mostwins]

comparisonPicks[,outright]
comparisonPicks[,mostwins]

pick <- comparisonPicks[,mostwins]
straightPicks <- weekFile$Victor
straightPicks[pick == 0] <- weekFile$Underdog[pick == 0]

outDF <- data.frame(Game = weekFile$Game, pick = straightPicks)
outDF[order(weekFile$order), ]

