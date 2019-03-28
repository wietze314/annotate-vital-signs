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
    
    # prepare delete buttons here, since rowwise() hurts performance
    
    getCaseData(getCase = input$case) %>%
      rowwise() %>%
      mutate(delete = as.character(actionButton(
        paste0('button',id),
        label = "Delete",
        onclick = paste0('Shiny.onInputChange(\"select_button\",  this.id)') ))) %>%
      ungroup()
  }) 
  
  observe({
    artefacts$status <- data.frame(id = vitals()$id,
                                   status = FALSE,
                                   stringsAsFactors = FALSE)
  })
  
  # to change to dataframe with id and artefact status
  
  artefacts <- reactiveValues(
    numberofvitals = 1,
    status = data.frame(id = 1, 
                        status = FALSE,
                        stringsAsFactors = FALSE)
  )
  
  # Click to toggle artifact status
  
  observeEvent(input$VitalsPlot_click, {
    res <- nearPoints(vitals(), input$VitalsPlot_click, allRows = FALSE)
    
    toggle <- artefacts$status$id %in% res$id
    
    artefacts$status$status <- xor(artefacts$status$status, toggle)
  })
  
  # Drag to toggle artifact status brush
  
  observeEvent(input$VitalsPlot_brush, {
    res <- brushedPoints(vitals(), input$VitalsPlot_brush, allRows = FALSE)
    
    toggle <- artefacts$status$id %in% res$id
    
    artefacts$status$status <- xor(artefacts$status$status, toggle)
    
    # reset brush
    session$resetBrush("VitalsPlot_brush")
  })
  
  # change range of slider input to match duration of surgery
  
  observe({
    mintime <- floor(min(vitals()[,'time']))
    maxtime <- ceiling(max((vitals()[,'time']-60),mintime))
    
    updateSliderInput(session = session, inputId = "PlotTime", min = mintime,  max = maxtime)
    
  })
  
  # navigation through procedure
  
  observeEvent(input$TimePlus30, {
    updateSliderInput(session = session, inputId = "PlotTime", value = input$PlotTime+30)
  })
  
  observeEvent(input$TimeMin30, {
    updateSliderInput(session = session, inputId = "PlotTime", value = input$PlotTime-30)
  })
  
  # plot vitals and artefact status
  
  output$VitalsPlot <- renderPlot({
    plotvitals <- vitals()
    
    # combine vitals() and artefacts$status
    
    plotdat <- plotvitals %>% 
      left_join(artefacts$status, by = 'id') %>%
      mutate(artefact = status) %>%
      mutate(type = factor(match(type, vitaltypes$field), 
                           levels = seq_len(nrow(vitaltypes)), 
                           labels = vitaltypes$label))
    
    if((plotvitals %>% filter(grepl("nibp$",type)) %>% nrow) > 0){
      
      # separate dataset to plot errorbars (NIBP)
      
      nibpdat <- plotvitals %>% 
        filter(grepl("nibp$",type)) %>% 
        select(type, plotvalue, time) %>%
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
      geom_point(data = plotdat %>% filter(artefact), mapping = aes(x = time, y = plotvalue, color = type),
                 shape = 4, size = 2, stroke = 2) +
      theme_bw()
    
  })
  
  
  # display artefact information
  
  output$artefacts <- renderDataTable(
    
    vitals() %>%
      # should be changed to a join with id
      left_join(artefacts$status, by = 'id') %>%
      filter(status) %>%
      arrange(type, time) %>%
      group_by(type) %>%
      # display vital name only in first row per vital
      mutate(vital = if_else(row_number(time)==1,unlist(vitaltypes[match(type, vitaltypes$field),"label"]),""),
             time = as.integer(floor(time))) %>%
      ungroup() %>%
      select(id, vital, time, value, delete),
    escape = FALSE
  )
  
  observeEvent(input$select_button, {
    
    #reverse select id
    
    rowid <- str_extract(input$select_button,pattern="(?<=button).+")
    artefacts$status[artefacts$status$id == rowid,'status'] <- FALSE
  })
  
  output$debug <- renderPrint({
    
  })
  
  observeEvent(input$Save, {
    
    dat <- vitals()
    dat$artefact <- artefacts$status
    timestmp <- format(Sys.time(),"%Y%m%d_%H_%M_%S")
    saveRDS(dat,
            paste0("data/annotated_case_",input$case,"_",timestmp,".RDS"))
  })
  
})
