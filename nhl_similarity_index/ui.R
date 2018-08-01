# UI
# update with Z score

library(data.table)
library(shiny)
library(rsconnect)
library(plotly)

rsconnect::setAccountInfo(name='plus1assist',
                          token='1F74CE3BB77348329F6054F0FD62DEEC',
                          secret='0Qr1xIhXk16mN6b0GRg33RoNmedxHc5eJDkRU13s')


DT <- fread("NHL_polar_1718.csv")
cats <- setdiff(names(DT), c("name", "pos", "team", "GP", 
                             "K1", "K2", "K3", "K4", "K5", "K6"))

nSegments <- 16
theta <- seq(0, 360, by = 360/nSegments)[1:nSegments]


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  fluidRow( 
    column(width = 1),
    column(width = 10, # align = "center",
           h1("NHL Player Similarity Index", align = "center"),
           br(),
           "Inspired by FiveThirtyEight's",
           a("World Cup Player Comparison Tool", href = "https://projects.fivethirtyeight.com/world-cup-comparisons/"), 
           " and Ryan Stimson's work on ",
           a("NHL Play Style Clustering,", href = "https://hockey-graphs.com/2017/04/04/identifying-player-types-with-clustering/"),
           "here is a comparison tool for NHL players based on production, deployment, and competition."
    ),
    column(width = 1)
  ),
  fluidRow( # spacer row
    column(12, div(style = "height:30px"))
  ),
  fluidRow( 
    column(width = 1),
    column(width = 3, 
           # selectizeInput("name.selection", choices = DT$name[order(DT$name)], selected = "Connor McDavid", label = "Name")
           selectInput("name.selection", 
                       choices = DT$name[order(DT$name)], 
                       selected = NULL, 
                       label = "Choose Player")
    ),
    column(width = 7, 
           selectInput("class.selection", 
                       choices = unique(DT$class)[order(unique(DT$class))], 
                       selected = "17/18 only", 
                       label = "Choose Date Range")
    ),
    column(width = 1)
  ),
  fluidRow( 
    column(width = 1),
    column(width = 10, align = "center", plotlyOutput("main.plot", height = 600, width = 900)),
    column(width = 1)
  ),
  fluidRow( 
    column(width = 1),
    column(width = 10, 
           br(),
           p("This model evaluates and compares player performances across 16 metrics between the 2015",
             HTML("&ndash;"), "2016 and 2017",
             HTML("&ndash;"), "2018 seasons. Each metric is measured on a per-game basis, and each uses a calculated a z-score",
             HTML("&mdash;"),
             "the number of standard deviations above / below average for that category.
             The model matches players who have a similar profile in different aspects parts of the game using the least mean squared error method, but ignores factors like age, height, weight, and position.
             "
             ),
           p("Quality of Teammate & Competition metrics were calculated using the point-per-game average of other skaters while on ice, weighted to the time spent playing with / against the target skater."
             
             ),
           p(
             "All metrics were calculated by",
             a("David Jung" , href = "https://twitter.com/plus1assist"),
             "using the NHL's play-by-play open game data via the",
             a("nhlscrapr", href = "https://cran.r-project.org/web/packages/nhlscrapr/index.html"),
             "package in R."
             )
           ),
           column(width = 1)
    ),
    fluidRow( # spacer row
      column(12, div(style = "height:50px"))
    )
  
))
