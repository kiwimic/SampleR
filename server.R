
library(shiny)
options(shiny.maxRequestSize = 200*1024^2) #200MB limit wielkości pliku do upload
source("global.R", encoding = "utf-8")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  ## Wczytanie listy arkuszy excel do reaktywnego wyboru w input$wybor_arkusza_xls i input$wybor_arkusza_xlsx####
  sheets_name <- reactive({
    if (!is.null(input$uploaded_file)) {
      file_ext <- stringr::str_extract(tolower(input$uploaded_file$datapath), pattern = "(\\.[a-z]+)$")
      if (file_ext %in% c(".xls", ".xlsx")) {
        return(excel_sheets(path = input$uploaded_file$datapath)) 
      }
    } else {
      return(NULL)
    }
  })
  ## Wczytanie listy kolumn do reaktywnego w wyborze kolumn do pokazania####
  full_data <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$wczytaj)
    file <- input$uploaded_file
    file_ext <-
      stringr::str_extract(tolower(input$uploaded_file$datapath), pattern = "(\\.[a-z]+)$")
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    ret <- tryCatch(
      {
        df <- switch(
          file_ext,
          ".xls" = {readxl::read_excel(path = file$datapath, sheet = input$wybor_arkusza_excel)},
          ".xlsx" = {readxl::read_excel(path = file$datapath, sheet = input$wybor_arkusza_excel)},
          ".csv" = {read.csv2(file = file$datapath, stringsAsFactors = F)}
        )
        
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(ret)
    
  })
  
  col_list <- reactive({
    req(input$wczytaj)
    
    return(colnames(full_data()))
   
  })
  
  
  output$opis_metody <- renderText({
    if (!is.null(input$wybor_metody)) {
      wybrana_metoda <- as.numeric(input$wybor_metody)
      str_wrap(OpisMetod_list[[wybrana_metoda]], 50)
    }
  })
  
  ## UI z lista kolumn####
  output$lista_kolumn_UI <- renderUI({
    req(input$wczytaj)
    
    if (length(col_list()) < 7) {
      selected <- col_list()
    } else {
      selected <- col_list()[c(1,2,3,
                               length(col_list())-2,
                               length(col_list())-1,
                               length(col_list()))]
    }
    selectizeInput(
      "wybor_kolumn",
      label = "Wybierz kolumny do wyświetlenia",
      choices = col_list(),
      selected = selected,
      multiple = TRUE,
      ##BUG https://github.com/rstudio/shiny/issues/1182
      options = list(placeholder = 'Kliknij aby wybrać kolumny do wyświetlenia')
    )
    
  })
  
  output$parametry_do_wcztania_pliku <- renderUI({
    if (is.null(input$uploaded_file))
      return()
    
    print(input$uploaded_file)
    file_ext <-
      stringr::str_extract(tolower(input$uploaded_file$datapath), pattern = "(\\.[a-z]+)$")
    print(file_ext)
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(
      file_ext,
      ".xls" = {
        list(
          selectizeInput(
            "wybor_arkusza_excel",
            label = "Wybierz arkusz xls",
            choices = sheets_name(),
            selected = character(0),
            multiple = TRUE,
            ##BUG https://github.com/rstudio/shiny/issues/1182
            options = list(placeholder = 'Kliknij aby wybrać arkusz',
                           maxItems = 1)
          ),
          actionButton("wczytaj", "Kliknij aby wczytać arkusz XLS")
        )
      },
      ".xlsx" = {
        list(
          selectizeInput(
            "wybor_arkusza_excel",
            label = "Wybierz arkusz xlsx",
            choices = sheets_name(),
            selected = character(0),
            multiple = TRUE,
            ##BUG https://github.com/rstudio/shiny/issues/1182
            options = list(placeholder = 'Kliknij aby wybrać arkusz',
                           maxItems = 1)
          ),
          actionButton("wczytaj", "Kliknij aby wczytać arkusz XLSX")
        )
      },
      ".csv" = actionButton("wczytaj", "Kliknij aby wczytać plik CSV"),
      ".tsv" = actionButton("wczytaj", "Kliknij aby wczytać plik TSV"),
      ".txt" = actionButton("wczytaj", "Kliknij aby wczytać plik TXT")
    )
    
  })
  
  output$wybor_parametrow_UI <- renderUI({
    req(input$wczytaj)
    
    if (is.null(input$wybor_metody))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$wybor_metody,
           "1" = {textInput("p1_wyborProcent",
                           label = "Wpisz wielkość próby w procentach",
                           value = NA,
                           placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%')},
           "2" = {textInput("p1_wyborProcent2",
                            label = "Wpisz wielkość próby w procentach",
                            value = NA,
                            placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%')},
           "3" =  {textInput("p1_wyborProcent3",
                             label = "Wpisz wielkość próby w procentach",
                             value = NA,
                             placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%')},
           "4" = {textInput("p1_wyborProcent4",
                            label = "Wpisz wielkość próby w procentach",
                            value = NA,
                            placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%')},
           "5" = {textInput("p1_wyborProcent5",
                            label = "Wpisz wielkość próby w procentach",
                            value = NA,
                            placeholder = '1 oznacza 1%, 0.5/0,5 Pół procenta, 5 to 5%')}
    )
  })
  
  output$selectizeDoWyboruMetody <- renderUI({
    
    req(input$wczytaj)
    
    selectizeInput("wybor_metody",
                   label = "Wybór metody próbkowania: ",
                   choices = c("Próba losowa"=1,
                               "Próba losowa monetarna"=2,
                               "Znajdź conajmniej 1 nieprawidłowość"=3,
                               "Próba losowa w grupach"=4,
                               "Próba losowa z percentylami"=5),
                   selected = character(0), 
                   multiple = TRUE, ##BUG https://github.com/rstudio/shiny/issues/1182
                   options = list(placeholder = 'Kliknij aby wybrać jedną z metod próbkowania i przeczytać jej opis',
                                  maxItems = 1))
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
  


  output$render_table <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
     req(input$wczytaj)
     req(input$wybor_kolumn)
  
     return(full_data()[input$wybor_kolumn])
     
  #   file <- input$uploaded_file
  #   file_ext <-
  #     stringr::str_extract(tolower(input$uploaded_file$datapath), pattern = "(\\.[a-z]+)$")
  #   
  #   # when reading semicolon separated files,
  #   # having a comma separator causes `read.csv` to error
  #   ret <- tryCatch(
  #     {
  #       df <- switch(
  #         file_ext,
  #         ".xls" = {readxl::read_excel(path = file$datapath, sheet = input$wybor_arkusza_excel)},
  #         ".xlsx" = {readxl::read_excel(path = file$datapath, sheet = input$wybor_arkusza_excel)},
  #         ".csv" = {read.csv2(file = file$datapath, stringsAsFactors = F)}
  #         )
  #           
  #         
  #     },
  #     error = function(e) {
  #       # return a safeError if a parsing error occurs
  #       stop(safeError(e))
  #     }
  #   )
  #   
  #   return(ret)
  #   
 })
  
  
})
