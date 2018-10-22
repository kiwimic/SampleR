library(shiny)

shiny::runApp(appDir = getwd(), host = "192.168.42.68", port = 8080, launch.browser = T)
