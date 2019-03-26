
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
  
  # structure(list(time = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 
  #                         14, 15, 16, 17, 18, 19, 20, 21, 22, 3, 4, 5, 6, 7, 8, 9, 10, 
  #                         11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 4, 7, 10, 12, 
  #                         14, 16, 18, 21, 22, 23, 25, 26, 27, 29, 30, 31, 32, 34, 35, 36, 
  #                         4, 7, 10, 12, 14, 16, 18, 21, 22, 23, 25, 26, 27, 29, 30, 31, 
  #                         32, 34, 35, 36, 4, 7, 10, 12, 14, 16, 18, 21, 22, 23, 25, 26, 
  #                         27, 29, 30, 31, 32, 34, 35, 36), 
  #                type = c("heartrate", "heartrate", 
  #                         "heartrate", "heartrate", "heartrate", "heartrate", "heartrate", 
  #                         "heartrate", "heartrate", "heartrate", "heartrate", "heartrate", 
  #                         "heartrate", "heartrate", "heartrate", "heartrate", "heartrate", 
  #                         "heartrate", "heartrate", "heartrate", "saturation", "saturation", 
  #                         "saturation", "saturation", "saturation", "saturation", "saturation", 
  #                         "saturation", "saturation", "saturation", "saturation", "saturation", 
  #                         "saturation", "saturation", "saturation", "saturation", "saturation", 
  #                         "saturation", "saturation", "saturation", "sysnibp", "sysnibp", 
  #                         "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", 
  #                         "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", 
  #                         "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", "sysnibp", 
  #                         "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", 
  #                         "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", 
  #                         "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", "meannibp", 
  #                         "meannibp", "meannibp", "dianibp", "dianibp", "dianibp", "dianibp", 
  #                         "dianibp", "dianibp", "dianibp", "dianibp", "dianibp", "dianibp", 
  #                         "dianibp", "dianibp", "dianibp", "dianibp", "dianibp", "dianibp", 
  #                         "dianibp", "dianibp", "dianibp", "dianibp"), 
  #                value = c(97, 101, 
  #                          92, 95, 85, 93, 87, 87, 87, 92, 93, 90, 88, 83, 82, 72, 68, 62, 
  #                          66, 83, 98.3, 98, 98.3, 98, 98.9, 98.5, 99.8, 99.2, 99, 99.4, 
  #                          98.8, 98.7, 99, 94.7, 98, 98.5, 95.9, 98.1, 99.1, 98.2, 142, 
  #                          132, 126, 128, 136, 107, 107, 108, 121, 87, 102, 107, 100, 112, 
  #                          115, 114, 110, 102, 103, 105, 93, 86, 86, 86, 70, 70, 82, 76, 
  #                          76, 51, 57, 62, 66, 63, 70, 75, 65, 64, 71, 65, 71, 64, 72, 74, 
  #                          57, 55, 74, 61, 59, 32, 31, 55, 50, 47, 48, 58, 48, 48, 61, 50
  #                ), case = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  #                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  #                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  #                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
  #                            1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)), 
  #           class = c("tbl_df", 
  #                     "tbl", "data.frame"), .Names = c("time", "type", "value", "case"
  #                     ), row.names = c(NA, -100L)) %>%
  #   filter(case == getCase, value > 0) %>%
  #   # special display modus (saturation on top as in AIMS)
  #   mutate(plotvalue = if_else(type == "saturation",3*value,value))
  # 
  
}

# collect case data
cases <- 1:10


