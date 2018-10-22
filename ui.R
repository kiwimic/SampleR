library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("SampleR"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput('uploaded_file', 'Wybierz plik do wczytania',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv',
                  '.xlsx',
                  ".xls"
                )
                ),
      uiOutput("parametry_do_wcztania_pliku"),
      uiOutput("lista_kolumn_UI"),
      uiOutput("selectizeDoWyboruMetody"),
      # selectizeInput("wybor_metody",
      #                label = "Wybór metody próbkowania: ",
      #                choices = c("Próba losowa"=1,
      #                            "Próba losowa monetarna"=2,
      #                            "Znajdź conajmniej 1 nieprawidłowość"=3,
      #                            "Próba losowa w grupach"=4,
      #                            "Próba losowa z percentylami"=5),
      #                selected = character(0), 
      #                multiple = TRUE, ##BUG https://github.com/rstudio/shiny/issues/1182
      #                options = list(placeholder = 'Kliknij aby wybrać jedną z metod próbkowania i przeczytać jej opis',
      #                               maxItems = 1)),
      
      verbatimTextOutput("opis_metody"),
      uiOutput("wybor_parametrow_UI"),
      verbatimTextOutput("test_dynamiczny")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      dataTableOutput("render_table")
    )
  )
))

