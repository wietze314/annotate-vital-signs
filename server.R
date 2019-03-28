#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  
  # for input buttons table
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(paste0(id, i), ...))
    }
    inputs
  }
  
  vitals <- reactive({
    getCaseData(getCase = input$case)
  }) 
  
  observe({
    n <- nrow(vitals())
    artefacts$status <- list()
    artefacts$numberofvitals <- n
    artefacts$status <- rep(FALSE,n)
  })
  
  # numberofvitals <- nrow(vitals())
  # numberofvitals <- 215
  
  # to change to dataframe with id and artefact status
  
  artefacts <- reactiveValues(
    numberofvitals = 1,
    status = rep(FALSE, 1)
  )
  
  # Click to toggle artifact status
  
  observeEvent(input$VitalsPlot_click, {
    res <- nearPoints(vitals(), input$VitalsPlot_click, allRows = TRUE)[1:artefacts$numberofvitals,]
    
    artefacts$status <- xor(artefacts$status, res$selected_)
  })
  
  # Drag to toggle artifact status brush
  
  observeEvent(input$VitalsPlot_brush, {
    res <- brushedPoints(vitals(), input$VitalsPlot_brush, allRows = TRUE)[1:artefacts$numberofvitals,]
    
    artefacts$status <- xor(artefacts$status, res$selected_)
    
    session$resetBrush("VitalsPlot_brush")
  })
  
  observe({
    mintime <- floor(min(vitals()[,'time']))
    maxtime <- ceiling(max((vitals()[,'time']-60),mintime))
    
  # change range of slider input to match duration of surgery
    updateSliderInput(session = session, inputId = "PlotTime", min = mintime,  max = maxtime)
    
  })
  
  observeEvent(input$TimePlus30, {
    updateSliderInput(session = session, inputId = "PlotTime", value = input$PlotTime+30)
  })
  
  observeEvent(input$TimeMin30, {
    updateSliderInput(session = session, inputId = "PlotTime", value = input$PlotTime-30)
  })
  
  output$VitalsPlot <- renderPlot({
    plotvitals <- vitals()
    if(nrow(plotvitals)== artefacts$numberofvitals){
      plotvitals$artefact <- artefacts$status
    } else {
      plotvitals$artefact <- FALSE
    }
    plotdat <- plotvitals %>% 
      mutate(type = factor(match(type, vitaltypes$field), 
                           levels = seq_len(nrow(vitaltypes)), 
                           labels = vitaltypes$label))
    if((plotvitals %>% filter(grepl("nibp$",type)) %>% nrow) > 0){
      nibpdat <- plotvitals %>% filter(grepl("nibp$",type)) %>% 
        select(-value, - artefact,-id) %>%
        spread(type, plotvalue) %>%
        mutate(type = factor(match("meannibp", vitaltypes$field), 
                             levels = seq_len(nrow(vitaltypes)), 
                             labels = vitaltypes$label),
               plotvalue = meannibp)
      
    } else {
      nibpdat <- data.frame(time = as.numeric(NA), 
                            type = NA, 
                            meannibp = as.numeric(NA), 
                            dianibp = as.numeric(NA), 
                            sysnibp = as.numeric(NA), 
                            plotvalue = as.numeric(NA))
    }
    
    
    plotid <- input$case
    
    ggplot(plotdat,
           aes(x = time, y = plotvalue, color = type)) +
      labs(title = paste0("vitals from ",plotid)) +
      geom_errorbar(data = nibpdat, 
                    aes(x = time,
                        ymin = dianibp, 
                        ymax = sysnibp), 
                    position = position_dodge(.1)) +
      scale_color_manual(values = vitalpalette) +
      coord_cartesian(ylim = c(0, 300),
                      xlim = c(input$PlotTime,input$PlotTime+60)) +
      geom_point() +
      geom_line(data = plotdat %>% filter(!grepl("NIBP$", type))) +
      # mark artefacts
      geom_point(data = plotdat %>% filter(artefact),mapping = aes(x = time, y = plotvalue, color = type),
                 shape = 4, size = 2, stroke = 2) +
      theme_bw()
    
  })
  
  
  # still gives an error when there are no artefacts marked.
  
  output$artefacts <- renderDataTable(
    vitals() %>%
      # should be changed to a join with id
      filter(artefacts$status) %>%
      arrange(type, time) %>%
      group_by(type) %>%
      mutate(vital = if_else(row_number()==1,unlist(vitaltypes[match(type, vitaltypes$field),"label"]),""),
             time = as.integer(floor(time))) %>%
      ungroup() %>%
      select(id, vital, time, value) %>%
      rowwise() %>%
      mutate(delete = as.character(actionButton( 
                                 paste0('button',id), 
                                 label = "Delete", 
                                 onclick = paste0('Shiny.onInputChange(\"select_button\",  this.id)') ))) %>%
      ungroup(),
      escape = FALSE
  )

  tstValue <- reactiveValues(artefact = '')
  
  observeEvent(input$select_button, {
    
    #reverse select id
    rowid <- str_extract(input$select_button,pattern="(?<=button).+")
    artefacts$status[vitals()$id == rowid] <- FALSE
    
    tstValue$artefact <- rowid
  })
  
  output$debug <- renderPrint({
    str(artefacts$status)
  })
  
  observeEvent(input$Save, {
    
    dat <- vitals()
    dat$artefact <- artefacts$status
    timestmp <- format(Sys.time(),"%Y%m%d_%H_%M_%S")
    saveRDS(dat,
            paste0("data/annotated_case_",input$case,"_",timestmp,".RDS"))
  })

  
})
