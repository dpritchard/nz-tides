#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)

base_url <- "https://www.linz.govt.nz/sites/default/files/docs/hydro/tidal-info/tide-tables/maj-ports/csv/"
ports <- list("Kaikoura" = paste0(base_url, "Kaikoura%20"),
              "Timaru" = paste0(base_url, "Timaru%20"),
              "Spit Wharf (Otago Harbour)" = paste0(base_url, "Spit%20Wharf%20"), 
              "French Bay (Akaroa Harbour)" = paste0(base_url, "Akaroa%20"))
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tide Tool"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        selectInput("port", "Port", choices = ports),
        dateInput("start_date", "Start Date", value = Sys.Date()),
        dateInput("end_date", "End Date", value = Sys.Date()+2),
        sliderInput("pred_interval", label = "Prediction Interval (mins)", 
                    min = 5, max = 60, value = 20, step = 5),
        actionButton("updateButton", label = "Update View", icon = icon("refresh")),
        hr(),
        downloadButton("report", "Generate report")
    ),

    mainPanel(
       plotOutput("plot")
    )
  )
))
