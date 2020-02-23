library(shinydashboard)

dashboardPage(title = "Seguimiento CoronaVirus",
  dashboardHeader(title  = "Seguimiento CoronaVirus", titleWidth = "50%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)
