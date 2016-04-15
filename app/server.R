
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
source('read_in.R')
rm(item)

# Set a random starting seed
set.seed(as.numeric(Sys.time()))

shinyServer(function(input, output) {
  
  item <- reactive ({
    if(input$button == 0)
    {
    return()
    }
    isolate({   
      recent$name[sample(1:nrow(recent), 1)]
    })
    
  })
  output$pw <- renderPrint({item()})

  output$plot1 <- renderPlot({
    hist(df$value)
    abline(v = input$dollars)
  })
})
