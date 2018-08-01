# Server side
# update with Z score

library(data.table)
library(shiny)
library(rsconnect)
library(plotly)

rsconnect::setAccountInfo(name='plus1assist',
                          token='1F74CE3BB77348329F6054F0FD62DEEC',
                          secret='0Qr1xIhXk16mN6b0GRg33RoNmedxHc5eJDkRU13s')


# load data and set variables
DT <- fread("NHL_polar_1718.csv")
cats <- setdiff(names(DT), c("name", "pos", "team", "GP", "class", 
                             "K1", "K2", "K3", "K4", "K5", "K6"))

nSegments <- 16
theta <- seq(0, 360, by = 360/nSegments)[1:nSegments]


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$main.plot <- renderPlotly({
    
    tgt.class <- input$class.selection
    
    tgt <- ifelse(nchar(input$name.selection) == 0, "Sidney Crosby", input$name.selection)
    
    tgt.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == tgt, eval(cats), with = F]), # runif(16), # rep(1, 16),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K1.name <- DT[class == tgt.class & name == tgt]$K1
    K1.display <- paste(DT[class == tgt.class & name == tgt]$K1, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K1]$team, ")", sep = "")
    K1.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K1.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K2.name <- DT[class == tgt.class & name == tgt]$K2
    K2.display <- paste(DT[class == tgt.class & name == tgt]$K2, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K2]$team, ")", sep = "")
    K2.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K2.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K3.name <- DT[class == tgt.class & name == tgt]$K3
    K3.display <- paste(DT[class == tgt.class & name == tgt]$K3, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K3]$team, ")", sep = "")
    K3.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K3.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K4.name <- DT[class == tgt.class & name == tgt]$K4
    K4.display <- paste(DT[class == tgt.class & name == tgt]$K4, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K4]$team, ")", sep = "")
    K4.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K4.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K5.name <- DT[class == tgt.class & name == tgt]$K5
    K5.display <- paste(DT[class == tgt.class & name == tgt]$K5, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K5]$team, ")", sep = "")
    K5.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K5.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    K6.name <- DT[class == tgt.class & name == tgt]$K6
    K6.display <- paste(DT[class == tgt.class & name == tgt]$K6, " (", DT[class == tgt.class & name == DT[class == tgt.class & name == tgt]$K6]$team, ")", sep = "")
    K6.plot <- data.table( 
      pct = unlist(DT[class == tgt.class & name == K6.name, eval(cats), with = F]),
      theta.start = theta,
      theta.end   = theta[c(2:nSegments,1)]
    )
    
    
    plot_ly(tgt.plot, type = "scatterpolar", mode = "lines") %>%
      
      # tgt outline
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      add_trace( r = ~c(0, 1, 1,0),   theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "lightgray", width = 1), showlegend = F) %>%
      
      # tgt player values
      add_trace( r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9], pct[9],   0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(color = "black"), showlegend = F, hoverinfo = "none") %>%
      
      # K1 player values
      add_trace( data = K1.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      # K1 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar2", hoverinfo = "none") %>%
      
      
      # K2 player values
      add_trace( data = K2.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar3", hoverinfo = "none") %>% 
      
      # K2 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar3", hoverinfo = "none") %>%
      
      # K3 player values
      add_trace(data = K3.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar4", hoverinfo = "none") %>% 
      
      # K3 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar4", hoverinfo = "none") %>%
      
      # K4 player values
      add_trace(data = K4.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar5", hoverinfo = "none") %>% 
      
      # K4 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar5", hoverinfo = "none") %>%
      
      # K5 player values
      add_trace(data = K5.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar6", hoverinfo = "none") %>% 
      
      # K5 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar6", hoverinfo = "none") %>%
      
      # K6 player values
      add_trace(data = K6.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), fill = "toself", fillcolor = "orange", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), fill = "toself", fillcolor = "lightblue", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0),   theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), fill = "toself", fillcolor = "seagreen", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), fill = "toself", fillcolor = "tomato", line = list(width = 0), showlegend = F, subplot = "polar7", hoverinfo = "none") %>% 
      
      # K6 tgt outline
      add_trace( data = tgt.plot, 
                 r = ~c(0, pct[1], pct[1], 0),   theta = ~c(0, theta.start[ 1], theta.end[ 1], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[2], pct[2], 0),   theta = ~c(0, theta.start[ 2], theta.end[ 2], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[3], pct[3], 0),   theta = ~c(0, theta.start[ 3], theta.end[ 3], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[4], pct[4], 0),   theta = ~c(0, theta.start[ 4], theta.end[ 4], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[5], pct[5], 0),   theta = ~c(0, theta.start[ 5], theta.end[ 5], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[6], pct[6], 0),   theta = ~c(0, theta.start[ 6], theta.end[ 6], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[7], pct[7], 0),   theta = ~c(0, theta.start[ 7], theta.end[ 7], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[8], pct[8], 0),   theta = ~c(0, theta.start[ 8], theta.end[ 8], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[9],  pct[9],  0), theta = ~c(0, theta.start[ 9], theta.end[ 9], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[10], pct[10], 0), theta = ~c(0, theta.start[10], theta.end[10], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[11], pct[11], 0), theta = ~c(0, theta.start[11], theta.end[11], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[12], pct[12], 0), theta = ~c(0, theta.start[12], theta.end[12], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      add_trace( r = ~c(0, pct[13], pct[13], 0), theta = ~c(0, theta.start[13], theta.end[13], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[14], pct[14], 0), theta = ~c(0, theta.start[14], theta.end[14], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[15], pct[15], 0), theta = ~c(0, theta.start[15], theta.end[15], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      add_trace( r = ~c(0, pct[16], pct[16], 0), theta = ~c(0, theta.start[16], theta.end[16], 0), line = list(color = "black", width = 1), showlegend = F, subplot = "polar7", hoverinfo = "none") %>%
      
      
      layout( polar = list( 
        domain = list(
          x = c(0.00,0.5),
          y = c(0.00,1.0)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar2 = list( 
        domain = list(
          x = c(0.60,0.80),
          y = c(0.70,1.00)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar3 = list( 
        domain = list(
          x = c(0.80,1.00),
          y = c(0.70,1.00)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar4 = list( 
        domain = list(
          x = c(0.60,0.80),
          y = c(0.35,0.65)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar5 = list( 
        domain = list(
          x = c(0.80,1.00),
          y = c(0.35,0.65)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar6 = list( 
        domain = list(
          x = c(0.60,0.80),
          y = c(0.00,0.30)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      polar7 = list( 
        domain = list(
          x = c(0.80,1.00),
          y = c(0.00,0.30)
        ),
        radialaxis = list( visible = F),
        angularaxis = list( visible = F, showticklabels = F, showline = F)
      ),
      
      title = paste("Who played most like ", tgt, "? (", tgt.class, ")", sep = ""),
      margin = list(t = 95, b = 99),
      # annotations - maybe try "add annotations for in the chart but put it out of bounds?
      annotations = list( x = c(0.74, #K1
                                0.96, #K2
                                0.74, #K3
                                0.96, #K4
                                0.74, #K5
                                0.96, #K6
                                0.52,-0.05, # TOI, G
                                0.50,-0.01, # PPTOI, A
                                0.44, 0.05, # SHTOI, PP
                                0.27, 0.15, # OZS, USAT
                                -0.05, 0.55, # HIT, TK
                                -0.01, 0.49, # BLK GV
                                0.05, 0.42, # PIM QoC
                                0.15, 0.27, # FW  QoT
                                0 # name
                            ),
                            y = c(1.05,  #K1
                                  1.05,  #K2
                                  0.695, #K3
                                  0.695, #K4
                                  0.30,  #K5
                                  0.30,  #K6
                                  0.59, 0.59, # TOI, G
                                  0.81, 0.81, # PPTOI, A
                                  0.97, 0.97, # SHTOI, PP
                                  1.07, 1.07, # OZS, USAT
                                  0.40, 0.40, # HIT TK
                                  0.19, 0.19, # BLK GV
                                  0.00, 0.00, # PIM QoC
                                 -0.05,-0.07, # FW  QoT
                                 -0.20 # name
                            ), 
                            text = c(K1.display,
                                     K2.display,
                                     K3.display,
                                     K4.display,
                                     K5.display,
                                     K6.display,
                                     "Time\nOn Ice", "Goals",
                                     "Power Play\nTime On Ice", "Assists",
                                     "Shorthanded\nTime On Ice", "Power Play",
                                     "O. Zone\nStarts (5v5)", "Unblocked\nShots",
                                     "Hits", "Takeaways",
                                     "Blocks", "Giveaways",
                                     "Penalty\nMins", "Quality of\nCompetition",
                                     "Faceoffs", "Quality of\nTeammates",
                                     "Twitter: @plus1assist"
                            ),
                            showarrow = F)
                            ) %>%
      config(displayModeBar = F, showlink = F)
  })
  
})
