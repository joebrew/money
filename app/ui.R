shinyUI(fluidPage(
  titlePanel("How much is your money worth?"),
  fluidRow(
    column(3, wellPanel(
      textInput("dollars", "Dollars", value = 100),
      actionButton("button", "Change item")
    )),
    column(6,
           # textOutput('pw'),
           h3(textOutput('equivalent')))
  )
))