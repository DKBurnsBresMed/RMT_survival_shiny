

options(shiny.fullstacktrace = TRUE)


ui     <- source("./ui/u_master.R")$value
server <- source("./server/s_master.R")$value


shinyApp(ui,server)
