rm(list = ls())
library(shiny); library(scales)
load("2015wk01.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) { # input <- data.frame(players = 100, first = 100, second = 75, third = 50)

  poolsize <- reactive({input$players})
  #results <- calcWinners(input$players)
  fanScoreSubset <- reactive({fanSubset[, 1:poolsize]})
  
  comparisonFirst <- reactive({comparisonPicksScores > apply(fanScoreSubset(), 1, max)})
  comparisonTiedorFirst <- reactive({comparisonPicksScores >= apply(fanScoreSubset(), 1, max)})
  
  outright <- reactive({which(colSums(comparisonFirst()) == max(colSums(comparisonFirst())))})
  mostwins <- reactive({which(colSums(comparisonTiedorFirst) == max(colSums(comparisonTiedorFirst)))})
  
  straightPicks <- reactive({
    data <- weekFile$Victor
    data[comparisonPicks[,outright()] == 0] <- weekFile$Underdog[comparisonPicks[,outright()] == 0]
    data
  }) 

  straightPicksSafe <- reactive({
    data <- weekFile$Victor
    data[comparisonPicks[,mostwins()] == 0] <- weekFile$Underdog[comparisonPicks[,mostwins()] == 0]
    data
  })

  outDF <- reactive({
    data <- data.frame(Game = weekFile$Game, pick = straightPicks())
    data[order(weekFile$order), ]
    })
  
  outDFSafe <- reactive({
    data <- data.frame(Game = weekFile$Game, pick = straightPicksSafe())
    data[order(weekFile$order), ]
  })

})
