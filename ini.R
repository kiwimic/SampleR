library(shiny)

shiny::runApp(appDir = getwd(), host = "192.168.42.68", port = 8081, launch.browser = T)
