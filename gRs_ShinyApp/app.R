# install.packages("devtools")
#devtools::install_github("jenright-git/gRs")
library(shiny)
library(bslib)
library(gRs)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- page_sidebar(title="Mann Kendall Analysis and Timeseries Plots",
                   sidebar = sidebar(
                     shiny::fileInput(inputId = "file_input", label = "Upload Esdat File", accept = ".xlsx"),
                     
                     uiOutput(outputId = "analyte_selector")
                   ),
                   
                   card(card_header("Mann-Kendall Trends"),
                     plotOutput("mann_kendall_heatmap"), full_screen = TRUE
                   ),
                   
                   card(card_header("Timeseries Plot"),
                        plotOutput("timeseries_plot"), full_screen = TRUE
                        )
                   
                   
                   )



# Define server logic required to draw a histogram
server <- function(input, output) {

  file_data <- reactive({
    
    file <- input$file_input
    if(!is.null(file)){data_processor(file$datapath)}
    
  })
  
  
  output$mann_kendall_heatmap <- renderPlot({
    req(file_data())
    
    file_data() %>% 
      filter(analyte %in% input$analyte_input) %>% 
      mann_kendall_test() %>% 
      mann_kendall_heatmap()
  })
  

  output$analyte_selector <- renderUI({
    
    req(file_data())
    selectInput("analyte_input", 
                label = "Select Analytes", 
                choices = file_data() %>% distinct(analyte),
                selected = file_data() %>% distinct(analyte))
    
  })
  
    
  output$timeseries_plot <- renderPlot({
    if(!is.null(file_data())){
    file_data() %>% 
      filter(analyte %in% input$analyte_input) %>% 
      timeseries_plot()
  }
  })
    
    
    
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
