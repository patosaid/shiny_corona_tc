library(shinydashboard)
library(DT)
library(shiny)
library(plotly)
library(highcharter)

dashboardPage(title = "The progress of 2019 Novel Coronavirus (2019‐nCoV) by Patricio Said",
  dashboardHeader(title  = "The progress of 2019 Novel Coronavirus (2019‐nCoV) by Patricio Said", titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      valueBoxOutput("confirmed_value"),
      valueBoxOutput("deaths_value"),
      valueBoxOutput("recovered_value")
    ),
    fluidRow(
      box(width = 2,
        DT::dataTableOutput("table_country_confirmed")
      ),
      box(width = 6,
        p("Click the play button to see the progress:"),
        plotlyOutput("world_animated")),
      box(width = 2, 
        DT::dataTableOutput("table_country_death")
      ),
      box(width = 2, 
        DT::dataTableOutput("table_country_recovered")
      )
    ),
    fluidRow(
      box(
        highchartOutput("plot1")
      ),
      box(
        highchartOutput("plot2")
      )
    )
  )
)
