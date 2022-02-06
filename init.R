#init.R

my_packages=c("httr","readxl","leaflet","shiny","shinydashboard","shinythemes","shinyWidgets","highcharter","bslib",
              "thematic","dplyr","ggplot2","htmltools","lubridate","scales","forcats","fresh")

install_if_missing=function(p){
  if(p %in% rownames(installed.packages())==FALSE){
    install.packages(p) 
     }
  }


invisible(sapply(my_packages,install_if_missing))

