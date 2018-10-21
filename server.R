library(shiny)
options(shiny.maxRequestSize = 200*1024^2) #200MB limit wielkości pliku do upload
source("global.R", encoding = "utf-8")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- 30
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'white')
    
  })
  
  sheets_name <- reactive({
    if (!is.null(input$uploaded_file)) {
      file_ext <- stringr::str_extract(tolower(input$uploaded_file), pattern = "(\\.[a-z]+)$")
      if (file_ext %in% c(".xls", ".xlsx")) {
        return(excel_sheets(path = input$uploaded_file$datapath)) 
      }
    } else {
      return(NULL)
    }
  })
  
  
  output$opis_metody <- renderText({
    if (!is.null(input$wybor_metody)) {
      wybrana_metoda <- as.numeric(input$wybor_metody)
      str_wrap(OpisMetod_list[[wybrana_metoda]], 50)
    }
  })
  
  output$parametry_do_wcztania_pliku <- renderUI({
    if (is.null(input$uploaded_file))
      return()
    
    print(input$uploaded_file)
    file_ext <- stringr::str_extract(tolower(input$uploaded_file$datapath), pattern = "(\\.[a-z]+)$")
    print(file_ext)
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(file_ext,
           ".xls" = {selectizeInput("wybor_arkusza_xls",
                           label = "Wybierz arkusz",
                           choices = sheets_name(),
                           selected = character(0), 
                           multiple = TRUE, ##BUG https://github.com/rstudio/shiny/issues/1182
                           options = list(placeholder = 'Kliknij aby wybrać arkusz',
                                          maxItems = 1))
             },
           ".xlsx" = {list(selectizeInput("wybor_arkusza_xlsx",
                                     label = "Wybierz arkusz",
                                     choices = sheets_name(),
                                     selected = character(0), 
                                     multiple = TRUE, ##BUG https://github.com/rstudio/shiny/issues/1182
                                     options = list(placeholder = 'Kliknij aby wybrać arkusz',
                                                    maxItems = 1)),
                           actionButton("xlsx_wczytaj", "Kliknij aby wczytać arkusz")
                           )
             },
           ".csv" =  numericInput("dynamic", "Dynamic",
                                     value = 12),
           ".tsv" = checkboxInput("dynamic", "Dynamic",
                                      value = TRUE),
           ".txt" = checkboxGroupInput("dynamic", "Dynamic",
                                                choices = c("Option 1" = "option1",
                                                            "Option 2" = "option2"),
                                                selected = "option2"
           )
    )
    
    
  })
  
  output$wybor_parametrow_UI <- renderUI({
    if (is.null(input$wybor_metody))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$wybor_metody,
           "1" = textInput("p1_wyborProcent",
                           label = "Wpisz wielkość próby w procentach",
                           value = NA,
                           placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%'),
           "text" = textInput("dynamic", "Dynamic",
                              value = "starting value"),
           "numeric" =  numericInput("dynamic", "Dynamic",
                                     value = 12),
           "checkbox" = checkboxInput("dynamic", "Dynamic",
                                      value = TRUE),
           "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",
                                                choices = c("Option 1" = "option1",
                                                            "Option 2" = "option2"),
                                                selected = "option2"
           ),
           "radioButtons" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
           ),
           "selectInput" = selectInput("dynamic", "Dynamic",
                                       choices = c("Option 1" = "option1",
                                                   "Option 2" = "option2"),
                                       selected = "option2"
           ),
           "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                               choices = c("Option 1" = "option1",
                                                           "Option 2" = "option2"),
                                               selected = c("option1", "option2"),
                                               multiple = TRUE
           ),
           "date" = dateInput("dynamic", "Dynamic"),
           "daterange" = dateRangeInput("dynamic", "Dynamic")
    )
    
   
  })
  output$test_dynamiczny <- renderText({
    if (!is.null(input$p1_wyborProcent)) {
      temp_string <- input$p1_wyborProcent
      print("Przed:")
      print(temp_string)
      temp_string %>%
        str_replace_all(pattern = ",", replacement = "\\.") %>%
        str_remove_all(pattern = "\\s") %>%
        #str_remove_all(pattern = "\\W") %>%
        str_remove_all(pattern = "[A-Za-z]") %>%
        as.numeric() -> temp_string
      print("Po:")
      print(temp_string)
      print(class(temp_string))
      
      temp_string <- temp_string/100
     return(paste0("Wybrałeś: ", temp_string * 100, "%"))
    }
  })
  
})
