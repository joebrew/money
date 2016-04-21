
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
  
  # Re-generated item
  item <- reactive ({
    if(input$button == 0)
    {
    return()
    }
    isolate({   
      recent$name[sample(1:nrow(recent), 1)]
    })
    
  })
  
  # Equivalent converter
  output$equivalent <- renderText({
    # Get the item
    if(input$button == 0){
      x <- recent$name[sample(1:nrow(recent), 1)]
    } else {
      x <- item()
    }
    
    # Get the value of the item
    value <- recent$value[grepl(x, recent$name)]
    # Divide the amount of money by the value
    divided <- as.numeric(input$dollars) / value
    # Convert to text
    out <- paste0(round(divided, digits = 2),
                  ' pounds of ', 
                  tolower(x))
    
  })

})
