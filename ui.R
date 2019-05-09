#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "shiny.css",
  
  # Application title
  titlePanel("Annotate your data now"),
  
  # Sidebar with a slider input for number of bins 
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dos",
                  "select date of surgery",
                  choices = getDateList()),
      selectInput(inputId = "case",
                  label = "Select case:",
                  choices = getCaseList(min(getDateList()))),
      textAreaInput("remarks","Remarks"),
      actionButton("Save","Save"),
      textOutput("FileStatus")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("VitalsPlot", click = "VitalsPlot_click", brush = "VitalsPlot_brush"),
      splitLayout(cellWidths = c("10%", "80%","10%"),
                  actionButton("TimeMin30", "-30"),
                  sliderInput("PlotTime", min = 0, max = 0, value = 0, label = "Time", step = 1,
                              width = "95%"),
                  actionButton("TimePlus30", "+30")
      ),
      h2("Marked Artefacts"),
      dataTableOutput("artefacts"),
      verbatimTextOutput("debug")
      
    )
    
  )
))
