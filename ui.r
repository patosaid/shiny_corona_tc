library(shinydashboard)
library(DT)
library(shiny)
library(plotly)
library(highcharter)

dashboardPage(title = "The progress of 2019 Novel Coronavirus (2019‐nCoV) by Patricio Said",
              skin = "red",
  dashboardHeader(title  = "The progress of 2019 Novel Coronavirus (2019‐nCoV) by Patricio Said", titleWidth = "100%"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(tags$head(includeHTML(("google.html"))),
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
        plotlyOutput("world_animated", width = "100%")),
      box(width = 2, 
        DT::dataTableOutput("table_country_death")
      ),
      box(width = 2, 
        DT::dataTableOutput("table_country_recovered")
      )
    ),
    fluidRow(
      box(title = "Cumulative cases",
        highchartOutput("plot1")
      ),
      box(title = "Daily cases",
        highchartOutput("plot2")
      )
    ),
    fluidRow(
      column( width = 12, 
        textOutput("day_updated"),
        p("Data source from GitHub ", 
          a(href = "https://github.com/CSSEGISandData/COVID-19", "here"), ".",
          br(),
          "Made by",a(href="https://twitter.com/Patosaid_cl", "Patricio Said") ,
          "with Shiny, Highcharter, Plotly and DataTables.")
      )
    )
  )
)
