#get necessary functions and options for calculations and user selection
source('~/global.R')

#rsconnect::setAccountInfo(name='emilhs', token='FA6EADF54D443B382672A13AB86641C2', secret='miBtDWzaeYuUULJ3MhV6kg+d235KpCL9Ecu/IZEz')

source('~/ui.R', local = TRUE)
source('~/server.R')

shinyApp(
  ui = my_ui,
  server = my_server
)