library(shiny)
library(rsconnect)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(epiR)
library(pwr)
library(powerSurvEpi)

#get necessary functions and options for calculations and user selection
source('~/funs.R')

#rsconnect::setAccountInfo(name='emilhs', token='FA6EADF54D443B382672A13AB86641C2', secret='miBtDWzaeYuUULJ3MhV6kg+d235KpCL9Ecu/IZEz')

source('~/ui.R', local = TRUE)
source('~/server.R')

shinyApp(
  ui = my_ui,
  server = my_server
)