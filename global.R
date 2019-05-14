
# load packages
require(shiny)
require(dplyr)
require(ggplot2)
require(purrr)
require(tidyr)
require(stringr)
require(lubridate)

set.seed(314)

vitaltypes <- tribble(
  ~field, ~label, ~color,
  "sysnibp", "systolic NIBP", "0000FF",
  "meannibp", "mean NIBP", "0000FF",
  "dianibp", "diastolic NIBP", "0000FF",
  "sysabp", "systolic IBP", "730C5A",
  "meanabp", "mean IBP", "E5BFDE",
  "diaabp", "diastolic IBP", "730C5A",
  "heartrate", "heartrate", "FF0000",
  "saturation", "saturation", "42BEFF"
)

vitalpalette <- paste0("#",vitaltypes$color)
names(vitalpalette) <- vitaltypes$label



# data generation

if(file.exists("functions/data.R"))
{
  source("functions/data.R")
} else {
  # use example data

  # example data is simulated
  # this function should be replaced by a function that collects the actual cases from the AIMS
  
  procedures <- c('heart',
                  'right leg',
                  'left leg',
                  'right arm',
                  'levt arm',
                  'right eye',
                  'left eye',
                  'abdomen',
                  'brain')
  ncase <- 100
  
  allcaseinfo <- data.frame(id = 1:ncase,
                            dos = as.POSIXct('2018-04-01 9:00') + 
                              days(sample(-10:10,ncase, replace = T)) +
                              hours(sample(0:8, ncase, replace = T)),
                            procedure = paste('surgery on', sample(procedures,ncase, replace = T)),
                            stringsAsFactors = FALSE)
  
  # get list of available dates
  
  getDateList <- function(){
    min <- allcaseinfo$dos %>% as.Date %>% min
    max <- allcaseinfo$dos %>% as.Date %>% max
    dat <- data.frame(dos = min + days(0:as.numeric(difftime(max,min),units = "days"))) %>%
      left_join(allcaseinfo %>% mutate(dos = as.Date(dos)), by = 'dos') %>%
      group_by(dos) %>%
      summarise(n = sum(!is.na(id))) %>%
      mutate(label = paste0(format(dos, "%Y/%m/%d"), " (",n,")"))
    setNames(format(dat$dos,"%Y/%m/%d"), dat$label)
  }
  
  getCaseList <- function(date){
    dat <- allcaseinfo %>%
      filter(format(dos,"%Y/%m/%d") == date)
    setNames(dat$id, paste(dat$id, format(dat$dos,"%H:%M"), dat$procedure, sep = " - "))
  }
  
  getCaseData <- function(getCase = 1){
    
    # example data is simulated
    # this function should be replaced by a function that collects the actual data from the AIMS
    
    set.seed(getCase)
    
    vitals <- data.frame(time = 1:150,
                         heartrate = round(rnorm(150,60, 5)),
                         saturation = 100 - sample(0:9,
                                                   150,
                                                   replace = TRUE,
                                                   prob = c(.8,.05,.05,.02,.02,.02,.01,.01,.01,.01))) %>%
      gather(type, value, - time) 
    
    vitalsnibp <- data.frame(time = 1:30*5-2,
                             sysnibp = round(rnorm(30, 
                                                   120,
                                                   10))) %>%
      mutate(dianibp = round(sysnibp - rnorm(30,50,5)),
             meannibp = round((2 * dianibp+sysnibp)/3)) %>%
      gather(type, value, - time) 
    
    vitals %>%
      bind_rows(vitalsnibp) %>%
      mutate(case = as.character(getCase)) %>%
      inner_join(allcaseinfo %>% mutate(id = as.character(id)), by = c('case' = 'id')) %>%
      # special display modus (local preference saturation on top in AIMS)
      mutate(plotvalue = if_else(type == "saturation",3*value,value)) %>%
      # simulate measurement identifier from data source
      mutate(id = paste0(type, time)) %>%
      mutate(time = dos + minutes(time))
    
    
  }
  
}

