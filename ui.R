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
                  choices = getDateList(),
                  selected = max(getDateList())),
      selectInput(inputId = "case",
                  label = "Select case:",
                  choices = getCaseList(max(getDateList()))),
      textAreaInput("remarks","Remarks"),
      actionButton("Save","Save"),
      textOutput("FileStatus")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("VitalsPlot", click = "VitalsPlot_click", brush = "VitalsPlot_brush"),
      splitLayout(cellWidths = c(rep("10%",3), "40%",rep("10%",3)),
                  actionButton("TimeMin60", "-60"),
                  actionButton("TimeMin30", "-30"),
                  actionButton("TimeMin10", "-10"),
                  textInput("PlotTime", value = 0, label = "Time"),
                  actionButton("TimePlus10", "+10"),
                  actionButton("TimePlus30", "+30"),
                  actionButton("TimePlus60", "+60")
      ),
      radioButtons("zoom",
                   label = "Zoom",
                   choices = c("10 minutes" = 10,
                               "30 minutes" = 30,
                               "60 minutes" = 60,
                               "2 hours" = 120),
                   selected = 60,
                   inline = TRUE),
      h2("Marked Artefacts"),
      dataTableOutput("artefacts"),
      verbatimTextOutput("debug")
      
    )
    
  )
))
