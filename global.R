
# load packages
require(shiny)
require(dplyr)
require(ggplot2)
require(purrr)
require(tidyr)
require(stringr)

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


getCaseData <- function(getCase = 1){
  
  set.seed(getCase)
  
  vitals <- data.frame(time = 1:150,
                       heartrate = round(rnorm(150,60, 5)),
                       saturation = 100 - sample(0:10,
                                                 150,
                                                 replace = TRUE,
                                                 prob = 10:0)) %>%
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
    mutate(case = getCase) %>%
    # special display modus (local preference saturation on top in AIMS)
    mutate(plotvalue = if_else(type == "saturation",3*value,value)) %>%
    # simulate measurement identifier from data source
    mutate(id = paste0(type, time))
  
 
}

# collect case data
cases <- 1:10


