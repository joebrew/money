shinyUI(fluidPage(
  titlePanel("How much is your money worth?"),
  fluidRow(
    column(3, wellPanel(
      textInput("dollars", "Dollars", value = 100),
      actionButton("button", "Get Data")
    )),
    column(6,
           textOutput('pw'),
           plotOutput("plot1", width = 400, height = 300)    
           )
  )
))