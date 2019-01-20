#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
  titlePanel(
    htmlOutput("strTitle")
  ),
  
  sidebarLayout(
    sidebarPanel(
       selectInput("dayID","Choose day:",
                   c("Monday","Tuesday","Wednesday","Thursday","Friday")),
       radioButtons("commuteType","Choose Mode of Transport",c("Bus","Car")),
       submitButton("Get the time!"),
       htmlOutput("usage")
    ),
    
    mainPanel(
      tabsetPanel(type="tabs",
                  tabPanel("Prediction",plotOutput("historicalPlot"),
                  fluidRow(
                    splitLayout(cellWidths = c("20%", "35%","45%"), 
                                h3("Prediction"), 
                                uiOutput("strFinalTime"),
                                h3("in minutes (linear model)"))
                    ),
                    htmlOutput("strArrivalTime")
                  ),
                  tabPanel("Documentation", htmlOutput("documentation")),
                  tabPanel("Data", h4("Dataset"),uiOutput("sourceData")),
                  tabPanel("Resources", h4("Other links"),uiOutput("resources"))
                  
      )
       #uiOutput("strFinalTime"),   #the table
       #h3("Likely arrival time"),
       #textOutput("strArrivalTime")
    )
  )
))
