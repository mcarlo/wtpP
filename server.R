rm(list = ls())
library(shiny); library(scales)
load("app2015wk01.RData")

# Define a server for the Shiny app
shinyServer(function(input, output) { # input <- data.frame(players = 100, first = 100, second = 75, third = 50, stringsAsFactors = F)

  poolsize <- reactive({input$players})
  #results <- calcWinners(input$players)
  sizeIndex <- reactive({poolsize()/5}) #poolsize/5
  dataDF <- reactive({playersBest[[2*sizeIndex()]]}) #playersBest[[2*sizeIndex]]
  
  outright <- reactive({dataDF()$outW}) #dataDF$outW
  outrightPicks <- reactive({dataDF()$outPicks}) #dataDF$outPicks
  
  mostwins <- reactive({dataDF()$mostW}) #dataDF$mostW
  mostWinsPicks <- reactive({dataDF()$mostPicks}) #dataDF$mostPicks
  
  straightPicks <- reactive({
    data <- weekFile$Victor
    data[outrightPicks() == 0] <- weekFile$Underdog[outrightPicks() == 0]
    #straightPicks <- 
    data
  }) 

  straightPicksSafe <- reactive({
    data <- weekFile$Victor
    data[mostWinsPicks() == 0] <- weekFile$Underdog[mostWinsPicks() == 0]
    #straightPicksSafe <- 
    data
  })

  outDF <- reactive({
    data <- data.frame(Game = weekFile$Game, pick = straightPicks(), stringsAsFactors = F)
    #outDF <- 
    data[order(weekFile$order), ]
    })
  
  outDFSafe <- reactive({
    data <- data.frame(Game = weekFile$Game, pick = straightPicksSafe(), stringsAsFactors = F)
    #outDFSafe <- 
    data[order(weekFile$order), ]
  })

})
